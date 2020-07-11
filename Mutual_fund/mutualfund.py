from pyquery import PyQuery as pq
import pandas as pd
import numpy as np
import urllib
from urllib.request import urlopen, Request
import time
import json
from datetime import datetime,date,timedelta
from tushare.stock import cons as ct
import os 
import threading
import sqlite3
from bs4 import BeautifulSoup
import re
os.chdir('/home/dwhang/project/MutualFund/')
lock = threading.Lock()
class SQLiteWraper(object):
    """
    数据库的一个小封装，更好的处理多线程写入
    """
    def __init__(self,path,command='',*args,**kwargs):  
        self.lock = threading.RLock() #锁  
        self.path = path #数据库连接参数  

        if command!='':
            conn=self.get_conn()
            cu=conn.cursor()
            cu.execute(command)

    def get_conn(self):  
        conn = sqlite3.connect(self.path)#,check_same_thread=False)  
        conn.text_factory=str
        return conn   

    def conn_close(self,conn=None):  
        conn.close()  

    def conn_trans(func):  
        def connection(self,*args,**kwargs):  
            self.lock.acquire()  
            conn = self.get_conn()  
            kwargs['conn'] = conn  
            rs = func(self,*args,**kwargs)  
            self.conn_close(conn)  
            self.lock.release()  
            return rs  
        return connection  

    @conn_trans    
    def execute(self,command,method_flag=0,conn=None):  
        cu = conn.cursor()
        try:
            if not method_flag:
                cu.execute(command)
            else:
                cu.execute(command[0],command[1])
            conn.commit()
        except sqlite3.IntegrityError as e:
            print (e)
            return -1
        except Exception as e:
            print (e)
            return -2
        return 0

    @conn_trans
    def fetchall(self,command="select name from xiaoqu",conn=None):
        cu=conn.cursor()
        lists=[]
        try:
            cu.execute(command)
            lists=cu.fetchall()
        except Exception as e:
            print (e)
            pass
        return lists

    
# def gen_fundmental_insert_command(info_dict):
#     """
#     """
#     info_list=[ '基金代码','基金全称','基金简称', '基金类型',  '份额规模', '资产规模',
#         '业绩比较基准','跟踪标的','发行日期', '成立日期/规模','成立来分红',
#         '基金托管人','托管费率', '基金管理人','管理费率', '销售服务费率']
#     #info_list=[]
#     t=[]
#     for il in info_list:
#         if il in info_dict:
#             t.append(info_dict[il])
#         else:
#             t.append('')
#     t=tuple(t)
#     command=(r"insert into fundmental_info values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",t)
#     return command 
 
# def gen_manager_insert_command(info_dict):
#     """
#     """
#     info_list=['起始期','截止期','基金经理','任职期间','任职回报','基金代码']
#     #info_list=[]
#     t=[]
#     for il in info_list:
#         if il in info_dict:
#             t.append(info_dict[il])
#         else:
#             t.append('')
#     t=tuple(t)
#     command=(r"insert into MutualFundManager values(?,?,?,?,?,?)",t)
#     return command 

####################basic fund level data ##################################    
def get_fund_id(tp='all',retry_count=3,pause=0.001):
    '''
    tp: string
    ['ETF-场内', 'QDII', 'QDII-ETF', 'QDII-指数', '保本型', '债券创新-场内', '债券型',
       '债券指数', '其他创新', '分级杠杆', '固定收益', '定开债券', '封闭式', '混合型', '理财型', '联接基金',
       '股票型', '股票指数', '货币型']
    '''
    url='http://fund.eastmoney.com/js/fundcode_search.js'
    datatype=['ETF-场内', 'QDII', 'QDII-ETF', 'QDII-指数', '保本型', '债券创新-场内', '债券型',
       '债券指数', '其他创新', '分级杠杆', '固定收益', '定开债券', '封闭式', '混合型', '理财型', '联接基金',
       '股票型', '股票指数', '货币型']
    if (tp!='all') & (tp not in datatype):
        print('input type error')
    for _ in range(retry_count):
        time.sleep(pause)
        try:
            request = Request(url)
            lines = urlopen(request, timeout = 10).read().decode('utf-8')
        except Exception as e:
            print(e)
        else:
            lines='{"data":'+lines[lines.index('[['):lines.rindex(']]')+2]+'}'
            lines=json.loads(lines)
            data=pd.DataFrame(lines['data'],columns=['基金代码','缩写','名称','基金类型','基金拼写'])
            data=data.sort_values('名称')
            data=data.drop_duplicates(subset='缩写')
            data=data.sort_values('基金代码')
            del data['基金拼写']
            if  tp=='all':
                return(data)
            elif tp in datatype:
                data=data[data['基金类型']==tp]
                data.index=range(0,data.shape[0])
                return(data)
    raise IOError(ct.NETWORK_URL_ERROR_MSG)
    
    
def get_fund_fundmental(code,db=None,retry_count=3,pause=0.01):
    tempdata=None
    temp=None
    url='http://fund.eastmoney.com/f10/jbgk_%s.html'% code
    #jjjl_url='http://fund.eastmoney.com/f10/jjjl_%s.html'% code
    for _ in range(retry_count):
        time.sleep(pause)
        try:
            request = Request(url)
            lines = urlopen(request, timeout = 10).read()
            doc=pq(lines.decode('utf-8'))
            data=pd.read_html(str(doc('table')))
            data=data[1]
            tempdata=np.append(data[[0,1]].values,data[[2,3]].values,axis=0)
            tempdata=pd.DataFrame(tempdata).set_index(0).T
            temp=tempdata.loc[1].to_dict()
        except Exception as e:
            print(e)
            return(False)
        else:
            if db:
                if isinstance(temp,dict):     
                    command=gen_fundmental_insert_command(temp)
                    db.execute(command,1)
                    return(True)
            else :
                return (tempdata)

    
# def get_fund_fumdmental_threading(codes,db=None):
#     for code in codes:
#         get_fund_fundmental(code,db)

def get_fund_manager(code,db=None,retry_count=3,pause=0.01):
    tempdata=None
    jjjl_url='http://fund.eastmoney.com/f10/jjjl_%s.html'% code
    for _ in range(retry_count):
        time.sleep(pause)
        #print('a')
        try:
            request = Request(jjjl_url)
            lines = urlopen(request, timeout = 10).read()
            doc=pq(lines.decode('utf-8'))
            data=pd.read_html(str(doc('table')))
            data=data[1]
            data['基金代码']=code
            tempdata=data.to_dict(orient='record')
        except Exception as e:
            print(e)
            return
        else:
            print(code)
            if db:
                for i in tempdata:
                    command=gen_manager_insert_command(i)
                    db.execute(command,1)
                    return
        #funds_manager.append(data)
            else:
                return(data)

# def get_fund_manager_threading(codes,db):
#     for code in codes:
#         get_fund_manager(code,db)

def get_manager_info(retry_count=3,pause=0.001):
    '''
    获取东方财富上基金经理的管理基金数据
    '''
    url='http://fund.eastmoney.com/Data/FundDataPortfolio_Interface.aspx?dt=14&mc=returnjson&ft=all&pn=2000&pi=1&sc=abbname&st=asc'
    for _ in range(retry_count):
        time.sleep(pause)
        try:
            request = Request(url)
            lines = urlopen(request, timeout = 10).read().decode('utf-8')
        except Exception as e:
            print(e)
        else:
            lines=lines[lines.index('{'):lines.index(']]')+2]+'}'
            data=lines.replace('data','"data"')
            DDa=pd.DataFrame(json.loads(data)['data'])
            DD=DDa[[0,1,3,6,7,10,11]].copy()
            DD.columns=['经理代码','基金经理','基金公司','从业时间','现任基金最佳回报(%)','管理规模(亿元)','历任基金最佳回报(%)']
            DD['管理基金数']=DDa[4].apply(lambda x: len(x.split(',')))
            DD['管理规模(亿元)']=DD['管理规模(亿元)'].apply(lambda x : float(x[0:x.index('亿')]) if x.find('亿')>0 else np.nan)
            DD[['现任基金最佳回报(%)','历任基金最佳回报(%)']]=DD[['现任基金最佳回报(%)','历任基金最佳回报(%)']].applymap(
                                                       lambda x : float(x[0:x.index('%')]) if x.find('%')>0 else np.nan)
            return(DD)
                   
def get_fund_rank(ft='gp',qdii='',sd='',ed='',pi=1,pn=100,retry_count=3,pause=0.001):
    """
        获取基金收益
    Parameters
    ------
      ft:string
          偏股：pg, 股票型：gp, 混合型：hh, 债券型：zq, 指数型:zs , 
          QDII:qdii, LOF: lof 
      qdii: num
           债券型——长期纯债:041,短期纯债:042,混合纯债:043, 定期开放债:008, 可转债:045
           指数型——沪深指数:053,行业指数:054,大盘指数:01, 中盘指数:02, 小盘指数:03,股票指数:001,债券指数:003
           QDII——全球股票:311,亚太股票：312,大中华区：313,新兴市场：314,金砖国家：315,美国股票：317,
                   全球指数：318,股债混合：320,债券：330, 商品：340
      sd:string
                  开始日期 format：YYYY-MM-DD 为空时取到去年今日的交易数据
      ed:string
                  结束日期 format：YYYY-MM-DD 为空时取到今日交易说句
      pn: num
          基金数
      retry_count : int, 默认 3
                 如遇网络等问题重复执行的次数 
      pause : int, 默认 0
                重复请求数据过程中暂停的秒数，防止请求间隔时间太短出现的问题
    return
    -------
    """
    url='http://fundapi.eastmoney.com/fundtradenew.aspx?ft=%s&rs=&gs=0&sc=zzf&st=desc&sd=%s&ed=%s&qdii=%s&tabSubtype=,,,,,&pi=%s&pn=%s'
    #url='http://fund.eastmoney.com/data/rankhandler.aspx?op=ph&dt=kf&ft=all&rs=&gs=0&sc=zzf&st=desc&sd=start&ed=end&qdii=&tabSubtype=,,,,,&pi=1&pn=7000&'
    colnames=['基金代码','基金简称','日期','单位净值','日增长率','近1周',
              '近1月','近3月','近6月','近1年','近2年' ,'近3年' ,'今年来','成立来']
    if (ed=='' )& (sd==''):
        ed=date.today().strftime('%Y-%m-%d')
        sd=(date.today()-pd.Timedelta(days=365)).strftime('%Y-%m-%d')
    url=url%(ft,sd,ed,qdii,pi,pn)
    for _ in range(retry_count):
        time.sleep(pause)
        try:
            request = Request(url)
            lines = urlopen(request, timeout = 10).read().decode('utf-8')
        except Exception as e:
            print(e)
        else:
            lines=lines[lines.index('{'):lines.index(']')+1]+']}'
            lines=lines.replace('|',',')
            data=lines.replace('","','"],["').replace(',','","').replace(']","[','],[').replace('datas:[','"datas":[[')
            js=json.loads(data)
            fundinfo=pd.DataFrame(js['datas'])
            fundinfo=fundinfo[[0,1]+list(range(3,15))]
            fundinfo.columns=colnames
            return(fundinfo)
    raise IOError(ct.NETWORK_URL_ERROR_MSG)      
    
    
    

    
    
    
##################### fund report download##################        
def get_quarter_report(url,retry_count=3,pause=0.01):
    for _ in range(retry_count):
        time.sleep(pause)
        try:
            request = Request(url)
            lines = urlopen(request, timeout = 10).read()
            doc=pq(lines.decode('utf-8'))
            data=pd.read_html(str(doc('table')),converters ={'股票代码':str,'债券代码':str})
            names=[x.text for x in BeautifulSoup(str(doc('label.right')),'lxml').findAll('font')]
        except Exception as e:
            continue
        else:
            data= [x.join(pd.DataFrame({'报告期':[y]})).fillna(method='ffill') for x,y in zip(data,names)]
            return(data)
    #raise IOError(ct.NETWORK_URL_ERROR_MSG)
    return


def get_fund_stocks(code,st=2010,detail=False):
    if detail:
        url='http://fund.eastmoney.com/f10/FundArchivesDatas.aspx?type=jjcc&code=%s&topline=10&year=%s&month=3,6,9,12'
    else:
        url='http://fund.eastmoney.com/f10/FundArchivesDatas.aspx?type=jjcc&code=%s&topline=10&year=%s'
    end=date.today().year-1
    col=['序号', '股票代码', '股票名称', '相关资讯', '占净值比例', '持股数（万股）', '持仓市值（万元）', '报告期']
    stocks_report=pd.DataFrame(columns=col)
    for year in range(end,st-1,-1):
        url_get=url%(code,year)
        #print(year)
        res=get_quarter_report(url_get)
        if res is None: 
            break
        else:
            stocks_report=stocks_report.append(res,ignore_index=True)
    return(stocks_report[col])
        
        
def get_fund_bonds(code,st=2010):
    url='http://fund.eastmoney.com/f10/FundArchivesDatas.aspx?type=zqcc&code=%s&topline=10&year=%s&month=3,6,9,12'
    end=date.today().year-1
    col=['序号', '债券代码', '债券名称', '占净值比例', '持仓市值（万元）', '报告期']
    bonds_report=pd.DataFrame(columns=col)
    for year in range(end,st-1,-1):
        url_get=url%(code,year)
        #print(year)
        res=get_quarter_report(url_get)
        if res is None :
            break
        else:
            bonds_report=bonds_report.append(res,ignore_index=True)
    return(bonds_report[col])

def get_fund_industry(code,st=2010):
    url='http://fund.eastmoney.com/f10/F10DataApi.aspx?type=hypz&code=%s&year=%s'
    end=date.today().year-1
    col=['序号', '行业类别', '行业变动详情', '占净值比例', '市值（万元）', '行业市盈率', '报告期']
    industry_report=pd.DataFrame(columns=col)
    for year in range(end,st-1,-1):
        url_get=url%(code,year)
        #print(year)
        res=get_quarter_report(url_get)
        if res is None :
            break
        else:
            industry_report=industry_report.append(res,ignore_index=True)
    return(industry_report[col])

##################################################
def get_fund_history(code=None,sdate='',edate='',page=1,per=40,retry_count=3,
            pause=0.001):
    """
        获取基金历史净值分红记录
    Parameters
    ------
      code:string
                  基金代码 e.g. 600848
      sdate:string
                  开始日期 format：YYYY-MM-DD 为空时取到API所提供的最早日期数据
      edate:string
                  结束日期 format：YYYY-MM-DD 为空时取到最近一个交易日数据
      retry_count : int, 默认 3
                 如遇网络等问题重复执行的次数 
      pause : int, 默认 0
                重复请求数据过程中暂停的秒数，防止请求间隔时间太短出现的问题
    return
    -------
      DataFrame
          属性:净值日期 单位净值 累计净值 日增长率 申购状态 赎回状态 分红送配
    """
    def get_fund_pg(url):
        doc=pq(url)
        data=pd.read_html(str(doc('table')))[0]
        #data=data.iloc[::-1]
        return(data)
    
    url='http://fund.eastmoney.com/f10/F10DataApi.aspx?type=lsjz&code=%s&page=%s&per=%s&sdate=%s&edate=%s&rt=0.08760689579229652'
    url_get=url%(code,page,per,sdate,edate)
    for _ in range(retry_count):
        time.sleep(pause)
        try:
            request = Request(url_get)
            lines = urlopen(request, timeout = 10).read().decode('utf-8')
            pg=int(re.findall('pages:(\d*),',lines)[0])
            #print(pg)
            #print(url_get)
        except Exception as e:
            print(e)
        else:
            data=pd.DataFrame(columns=['净值日期','单位净值','累计净值','日增长率','申购状态','赎回状态','分红送配'])
            for page in range(1,pg+1) :
                url_pg=url%(code,page,per,sdate,edate)
                temp=get_fund_pg(url_pg)
                data=data.append(temp,ignore_index=True)
                time.sleep(0.1)
            data=data.iloc[::-1]  
            return(data)
    raise IOError(ct.NETWORK_URL_ERROR_MSG)      
    
    
######################### operation function#######

def initial():
    con=sqlite3.connect('funds_database.db')
    fund_id=get_fund_id()
    fund_id.to_sql('FundsID',con,if_exists='replace',index=False)
    managerInfo=get_manager_info()
    managerInfo.to_sql('ManagerInfo',con,if_exists='replace',index=False)

def show_table(db='funds_database.db'):
    con=sqlite3.connect('funds_database.db')
    cur = con.cursor()
    cur.execute("select name from sqlite_master where type='table' order by name")
    temp=cur.fetchall()
    con.close()
    return(temp)

def history_mutualfund_initial():
    con=sqlite3.connect('funds_database.db')
    mutual_fund=pd.read_sql('select * from FundsID where 基金类型 in ("股票指数","股票型","混合型","债券型","债券指数","QDII-指数")',con)
    con.execute('drop table if exists MutualFundHistory')
    for code in mutual_fund['基金代码']:
        print(code)
        temp=get_fund_history(code)
        temp['基金代码']=code
        temp.to_sql('MutualFundHistory',con,if_exists='append',index=False)

def history_mutualfund_update():
    #initial()
    con=sqlite3.connect('funds_database.db')
    mutual_fund_new=temp=pd.read_sql('''select FundsID.基金代码 from FundsID left join (select 基金代码 from MutualFundHistory group by 基金代码) Code 
                                    on FundsID.基金代码=Code.基金代码 where FundsID.基金类型 in ("股票指数","股票型","混合型","债券型","债券指数","QDII-指数")
                                    and Code.基金代码 is null''',con)
    
    if ~mutual_fund_new.empty:
        for code in mutual_fund_new['基金代码']:
            print(code)
            temp=get_fund_history(code)
            temp['基金代码']=code
            temp.to_sql('MutualFundHistory',con,if_exists='append',index=False)
    mutual_fund=pd.read_sql('select 基金代码,max(净值日期) 日期 from MutualFundHistory group by 基金代码',con)
    mutual_fund['start']=mutual_fund['日期'].apply(lambda x: datetime.strftime(datetime.strptime(x,'%Y-%m-%d')+timedelta(days=1),'%Y-%m-%d'))
    for code,st in zip(mutual_fund['基金代码'],mutual_fund['start']):
        print(code,st)
        temp=get_fund_history(code,sdate=st)
        temp['基金代码']=code
        temp.to_sql('MutualFundHistory',con,if_exists='append',index=False)

        
def fundmental_mutualfund_update():
    con=sqlite3.connect('funds_database.db')
    mutual_fund=pd.read_sql('select * from FundsID where 基金类型 in ("股票指数","股票型","混合型","债券型","债券指数","QDII-指数")',con)
    col=['基金全称', '基金代码', '发行日期', '资产规模', '基金管理人', '基金经理人', '管理费率', '销售服务费率',
       '最高申购费率', '业绩比较基准', '基金简称', '基金类型', '成立日期/规模', '份额规模', '基金托管人', '成立来分红',
       '托管费率', '最高认购费率', '最高赎回费率', '跟踪标的']
    data=pd.DataFrame(columns=col)
    for code in mutual_fund['基金代码']:
        print(code)
        try:
            temp=get_fund_fundmental(code)
        except Exception as e:
            print(e)
        else:
            data=data.append(temp,ignore_index=True)
    data=data[col]
    con.execute('drop table if exists MutualFundFundmental')
    data.to_sql('MutualFundFundmental',con,if_exists='append',index=False)

def fundmanager_mutualfund_update():
    con=sqlite3.connect('funds_database.db')
    mutual_fund=pd.read_sql('select * from FundsID where 基金类型 in ("股票指数","股票型","混合型","债券型","债券指数","QDII-指数")',con)
    data=pd.DataFrame(columns=['起始期', '截止期', '基金经理', '任职期间', '任职回报', '基金代码'])
    for code in mutual_fund['基金代码']:
        print(code)
        try:
            temp=get_fund_manager(code)
        except Exception as e:
            print(e)
        else:
            data=data.append(temp,ignore_index=True)
    con.execute('drop table if exists MutualFundManager')
    data.to_sql('MutualFundManager',con,if_exists='append',index=False)
    
def fundrank_mutualfund_update():
    con=sqlite3.connect('funds_database.db')
    fund_return=pd.DataFrame()
    for ft in ['pg','gp','zs','hh','zq','qdii','fof']:
        temp=get_fund_rank(ft=ft,pn=10000)
        print(ft,temp.shape)
        fund_return=fund_return.append(temp,ignore_index=True)
    fund_return=fund_return.drop_duplicates()
    fund_manager=pd.read_sql('select * from MutualFundManager where 截止期="至今"',con)
    fund_mental=pd.read_sql('select 基金代码,资产规模,基金类型,基金管理人 from MutualFundFundmental',con)
    fund_mental['基金代码']=fund_mental['基金代码'].apply(lambda x: re.findall('\w*[0-9]',x)[0])
    base_info=['基金代码','基金简称','基金类型','基金管理人','资产规模',
                     '基金经理','起始期']
    select_info=['单位净值','近1周','近3月','近6月','近1年',
                       '近2年','近3年','成立来']
    fund_rank=pd.merge(pd.merge(fund_return,fund_manager,on='基金代码',how='inner'),fund_mental,on='基金代码',how='inner')
    def format_value(x):
        try:
            val=np.round(float(x),2)
            return(val)
        except Exception as e:
            return(np.nan)
    fund_rank[select_info]=fund_rank[select_info].applymap(format_value)
    fund_rank['资产规模']=fund_rank['资产规模'].apply(lambda x: float(str(x).replace("---","NaN").split("亿元")[0]))
    fund_rank['任职回报']=fund_rank['任职回报'].apply(lambda x: float( x.replace('%','')) if isinstance(x,str) else np.nan)
    fund_rank.to_sql('MutualFundRank',con,if_exists='replace',index=False)    

    
def fundposition_mutualfund_initial():
    con=sqlite3.connect('funds_database.db')
    data=pd.read_sql('select * from FundsID where 基金类型 in ("股票指数","股票型","混合型","债券型","债券指数","QDII-指数")',con)
    stock_list=stock['基金代码'].unique()
    bond_list=bond['基金代码'].unique()
    stocks_miss=[]
    bonds_miss=[]
    industry_miss=[]
    for i,code in enumerate(stock_list):
        tempdata=get_fund_stocks(code)
        if isinstance(tempdata,pd.DataFrame):
            tempdata['基金代码']=code
            if i==0:
                funds_stocks=tempdata
            else:
                funds_stocks=funds_stocks.append(tempdata)
            print('fininsh stock',code)
        else:
            stocks_miss.append(code)

        tempdata=get_fund_industry(code)
        if isinstance(tempdata,pd.DataFrame):
            tempdata['基金代码']=code
            if i==0:
                funds_industry=tempdata
            else:
                funds_industry=funds_industry.append(tempdata)
            print('fininsh industry',code)
        else:
            industry_miss.append(code)

    for i,code in enumerate(bond_list):
        tempdata=get_fund_bonds(code)
        if isinstance(tempdata,pd.DataFrame):
            tempdata['基金代码']=code
            if i==0:
                funds_bonds=tempdata
            else:
                funds_bonds=funds_bonds.append(tempdata)
            print('fininsh bond',code)
        else:
            bonds_miss.append(code)
    con=sqlite3.connect('funds_database.db')
    funds_bonds.drop_duplicates().set_index(['基金代码','报告期','序号']).to_sql('MutualFundbonds',con,if_exists='replace')
    funds_stocks.drop_duplicates().set_index(['基金代码','报告期','序号']).to_sql('MutualFundstocks',con,if_exists='replace')
    funds_industry.drop_duplicates().set_index(['基金代码','报告期','序号']).to_sql('MutualFundindustry',con,if_exists='replace')
##############################################        


def fundposition_mutualfund_update():
    year=date.today().year
    con=sqlite3.connect('funds_database.db')
    industry=pd.read_sql('select * from MutualFundindustry',con)
    stocks=pd.read_sql('select * from MutualFundstocks',con)
    bonds=pd.read_sql('select * from MutualFundbonds',con)
    data=pd.read_sql('select * from FundsID where 基金类型 in ("股票指数","股票型","混合型","债券型","债券指数","QDII-指数")',con)
    stock_list=stock['基金代码'].unique()
    bond_list=bond['基金代码'].unique()
    stocks_miss=[]
    bonds_miss=[]
    industry_miss=[]
    for i,code in enumerate(stock_list):
        tempdata=get_fund_stocks(code,st=year)
        if isinstance(tempdata,pd.DataFrame):
            tempdata['基金代码']=code
            stocks=stocks.append(tempdata)
            print('fininsh stock',code)
        else:
            stocks_miss.append(code)
            
        tempdata=get_fund_industry(code,st=year)
        if isinstance(tempdata,pd.DataFrame):
            tempdata['基金代码']=code
            industry=industry.append(tempdata)
        else:
            industry_miss.append(code)

    for i,code in enumerate(bond_list):
        tempdata=get_fund_bonds(code,st=year)
        if isinstance(tempdata,pd.DataFrame):
            tempdata['基金代码']=code
            bonds=bonds.append(tempdata)
            print('fininsh bond',code)
        else:
            bonds_miss.append(code)
            

    industry.drop_duplicates(subset=['基金代码','报告期','行业类别']).set_index(['基金代码','报告期','序号']).to_sql('MutualFundindustry',con,if_exists='replace')
    stocks.drop_duplicates(subset=['基金代码','报告期','股票名称']).set_index(['基金代码','报告期','序号']).to_sql('MutualFundstocks',con,if_exists='replace')
    bonds.drop_duplicates(subset=['基金代码','报告期','债券名称']).set_index(['基金代码','报告期','序号']).to_sql('MutualFundbonds',con,if_exists='replace')

   
    
if __name__ == '__main__':
    print('aa')
    print(os.getcwd())
    initial()
#    history_mutualfund_initial()
#    fundposition_mutualfund_initial()
    
    history_mutualfund_update()
    #fundmental_mutualfund_update()
    #fundmanager_mutualfund_update()
    fundrank_mutualfund_update()
    fundposition_mutualfund_update()
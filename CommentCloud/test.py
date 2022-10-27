import requests as rq
import pandas as pd
import json
import re
from os import path
import numpy as np
import os
import time


headers='''GET /list_detail_rate.htm?itemId=598433170230&sellerId=2104900096&currentPage=1 HTTP/1.1
Host: rate.tmall.com
User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:69.0) Gecko/20100101 Firefox/69.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2
Accept-Encoding: gzip, deflate, br
Referer: https://login.taobao.com/member/login.jhtml?redirectURL=https%3A%2F%2Frate.tmall.com%2Flist_detail_rate.htm%3FitemId%3D598433170230%26sellerId%3D2104900096%26currentPage%3D1
Connection: keep-alive
Cookie: lid=rhmily0627; cna=ScNBFEyUwlUCATr3YuJsVWeb; otherx=e%3D1%26p%3D*%26s%3D0%26c%3D0%26f%3D0%26g%3D0%26t%3D0; t=fa3e7b20123d61501879523e56e6855f; tracknick=rhmily0627; enc=kaaIvqhwwtLqejNGwVosAEU9NO5b0c2KbMv6qdZ82Gj3D%2FvONrdw5kW3Y6nWV2jY1LcenRNzeJWEpP6zhAbocQ%3D%3D; _tb_token_=37537b8bf660; cookie2=1de07c36f95afec7954b97e837bd0d0d; l=cBj3QfdHqBRzCODiBOCgNZwfuI_OSIOYYuWjlt_wi_5g-6T6gK7OksgFCF96VsWdOVLB4IqguYp9-etkweSNRTehl6ph.; isg=BBMTRsKHSrvumAbvCe7TU6hcoZc31sma3_SKvcUwbzJpRDPmTZg32nGVenSPf_-C; dnk=rhmily0627; uc1=cookie14=UoTaECEoqIo1Rg%3D%3D&cookie16=Vq8l%2BKCLySLZMFWHxqs8fwqnEw%3D%3D&existShop=false&cookie21=WqG3DMC9Fbxq&tag=8&pas=0&cookie15=W5iHLLyFOGW7aA%3D%3D&lng=zh_CN; uc3=lg2=VFC%2FuZ9ayeYq2g%3D%3D&nk2=EU90aKraUlbnig%3D%3D&vt3=F8dByuKxotSZugMUwXg%3D&id2=Vvz0JWcxsM4C; _l_g_=Ug%3D%3D; uc4=nk4=0%40E8uUrbJkNLcDNX5C39%2BeN6itVENg&id4=0%40VHrCOSzzhgxrPIflQvXHktB8g1Y%3D; unb=527798284; lgc=rhmily0627; cookie1=WvENINa6e8lWrWpsWjcIZEsRQFnbOa3ih5j2LJvPr1c%3D; login=true; cookie17=Vvz0JWcxsM4C; _nk_=rhmily0627; sg=74a; csg=ece0c074
Upgrade-Insecure-Requests: 1
Cache-Control: max-age=0'''

# 去除参数头尾的空格并按换行符分割
headers = headers.strip().split('\n')
 
# 使用字典生成式将参数切片重组，并去掉空格，处理带协议头中的://
headers = {x.split(':')[0].strip(): ("".join(x.split(':')[1:])).strip().replace('//', "://") for x in headers}
cookies=headers['Cookie']
cookies={x.split('=')[0].strip():("".join(x.split('=')[1:])).strip() for x in cookies.split(';')}
        
def get_TM_pinglun(url):
    #url='http://rate.tmall.com/list_detail_rate.htm?itemId= 41464129793&sellerId= 1652490016&currentPage= 1'
    print(url)
    myweb=rq.get(url,headers=headers,cookies=cookies)
    myjson=re.findall('"rateList".*',myweb.text)
    txt=re.findall('"rateList":(.*)',myweb.text)[0]
    reptxt=re.findall(',"searchinfo":"","from":"search","paginator":{"lastPage":\d*,"page":\d*,"items":\d*},"tags":\[\]}}\)',txt)[0]
    tt=txt.replace(reptxt,'').replace('[{','').replace('}]','').split('},{')
    Data=[]
    for i,a in enumerate(tt):
        temp='{"A":{'+a+'}}'
        temp=pd.read_json(temp).T
        if i==0:
            Data=temp
        else:
            Data=Data.append(temp,ignore_index=True)
    return(Data)



def get_TM_item_pinglun(itemId,sellerId,pages,file='word1.txt'):
    #itemId=re.findall(re.compile("itemId=\d*"),url)[0].replace('itemId=','')
    #sellerId=re.findall(re.compile("sellerId=\d*"),url)[0].replace('sellerId=','')
    #itemId=re.findall(re.compile('id=\d*'),url)[0].replace('id=','')
    #sellerId=re.findall(re.compile('user_id=\d*'),url)[0].replace('user_id=','')
    rawurl='http://rate.tmall.com/list_detail_rate.htm?itemId=%s&sellerId=%s&currentPage=%s'
    Data=[]
    unpage=list()
    for num in range(1,pages+1):
        time.sleep(1)
        url=rawurl%(itemId,sellerId,num)
        try:
            temp=get_TM_pinglun(url)
            #print(url)
            if len(Data)==0:
                Data=temp
            else:
                Data=Data.append(temp,ignore_index=True)
        except:
            unpage.append(num)
    #Data=Data[['rateContent','rateDate','goldUser','gmtCreateTime','tradeEndTime','useful']].drop_duplicates()
    Data['rateContent']=Data['rateContent'].apply(lambda x: x.replace('&hellip',''))
    os.chdir('/home/dwhang/project/Advancy/CommentCloud/Data')
    Data.to_csv(file, index=False, header=True,encoding='utf-8')
    print(unpage)
    return

def get_JD_pinglun(productId,page):
    s=rq.session()
    url = 'https://club.jd.com/comment/productPageComments.action'
    data = { 'productId':'1994763191',
            'score':0,
            'sortType':5,
            'pageSize':100,
            'isShadowSku':0,
            'page':0
    }
    data['productId']=productId
    data['page']=page
    r = s.get(url, params = data)
    txt=r.json()['comments']
    Data=list()
    for a in txt:
        Data.append(a)
    return(Data)
    
def get_JD_item_pinglun(url,pages,file='word1.txt'):
    itemId=re.findall('\d*[0-9]',url)[0]
    Data=list()
    unpage=list()
    for num in range(0,pages):
        time.sleep(1)
        try:
            temp=get_JD_pinglun(itemId,num)
            Data=Data+temp
        except:
            unpage.append(num)
    Data=pd.DataFrame(Data)
    os.chdir('/home/dwhang/project/Advancy/CommentCloud/Data')
    Data.to_csv(file,header=True,encoding='utf-8')
    print(unpage)
    return

def get_TB_pinglun(url):
    myweb=rq.get(url)
    txt=myweb.text
    txt=(re.findall('comments.*',txt))[0]
    tt=txt.split('"status":0}')
    Data=[]
    date=re.findall('"date".*?,',myweb.text)
    cont=re.findall('"content".*?",',myweb.text)
    Data=[]
    for a, b in zip(date,cont):
        temp=pd.read_json( '{"A":{'+a+b.replace('",','"')+'}}').T
        if len(Data)==0:
                Data=temp
        else:
                Data=Data.append(temp,ignore_index=True)
    return(Data)

def get_TB_item_pinglun(rawurl,pages):
    Data=[]
    unpage=list()
    for num in range(1,pages):
        time.sleep(2)
        url=rawurl%(num)
        try:
            temp=get_TB_pinglun(url)
            if len(Data)==0:
                Data=temp
            else:
                Data=Data.append(temp,ignore_index=True)
        except:
            unpage.append(num)
    print(unpage)
    return(Data)

import datetime
import time

etfindex={#'中证银行':'512800',
'中证消费':'159928',
'中证军工':'512660',
#'中证500':'510500',
'中证100':'159923',
'中小板指':'159902',
'证券公司':'512880',
'有色金属-指数':'000819',
'大宗商品':'399979',
'小康指数':'510160',
'细分医药':'512120',
'深证红利':'159905',
'深证成指':'159943',
#'深证100':'159901',
'上证医药':'510660',
'上证消费':'510630',
'上证50':'510050',
#'上证180':'510180',
'上海国企':'510810',
'全指医药':'159938',
'全指信息':'159939',
'全指金融':'159940',
'沪质城投':'511220',
'沪深300':'510300',
#'红利指数':'510880',
#'恒生指数':'159920',
#'恒生国企':'510900',
'地产指数-指数':'000006',
'创业板指':'159915',
'创业板50':'159949',
'5年国债':'511010',
'500医药':'512300',
'300非银':'512070',
'180金融':'510230',
'10年国债-指数':'000012',
'黄金':'518880',
'中证转债':'000832',
'06冀建投':'120602',
'06大唐债':'120601'}

def doSth():
    indexlist=[]
    dis=os.getcwd()+'/data/'
    for key in etfindex:
        code=etfindex[key]
        if code in ['000006','000012','000819','000832','399979']:
            tempdata=ts.get_k_data(code,index=True,start='2006-01-01', end='2017-12-31',autype='qfq')
        else:
            tempdata=ts.get_k_data(code,start='2006-01-01', end='2017-12-31',autype='qfq')
        tempdata=tempdata.set_index('date')
        indexlist.append(tempdata)
        tempdata.to_csv(dis+code+'.csv')


def main(h=0, m=0):

    '''h表示设定的小时，m为设定的分钟'''

    while True:

        # 判断是否达到设定时间，例如0:00

        while True:

            now = datetime.datetime.now()

            # 到达设定时间，结束内循环

            if now.hour==h and now.minute==m:

                break

            # 不到时间就等20秒之后再次检测

            time.sleep(20)

        # 做正事，一天做一次

        doSth()


if __name__ == '__main__':
    main()
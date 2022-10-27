library(shiny)
library(wordcloud2)

defaulturl='http://rate.tmall.com/list_detail_rate.htm?itemId=559534190631&sellerId=2928278102&currentPage=1'
defaulturlJD='https://item.jd.com/28195142923.html'
shinyUI({
  navbarPage("Items comment cloud: ",
             tabPanel('Tmall items',
              h2('Comment wordcloud'),
              fluidRow(
              column(10,textInput('url',strong('Input the Tmall items link below 
                                               (Please ensure that the link contain id number and user_id number) '),defaulturl,width='1600px')),
              
              column(2,numericInput('pages','Number of pages comments to crawl', 10,min=1,max=100))
              ),
    
              fluidRow(
              column(3,actionButton('run','Click the button to start the web crawl',icon=icon('play-circle'))),
              column(3,downloadButton('data',label='Download the comments data'))
              
              ),
              br(),
              fluidRow(
              column(4,uiOutput('status'))
              ),
              h3('Word cloud summary',align='c'),
              fluidRow(
              column(10,textInput('stopwords',strong('Input the words to exclude (Please use 、 to seperate the words)'),value='了、也、好、很、的、不、我、会、说、和、吧',width = '800px')),
              column(12,actionButton('recloud','Click the button to generate word cloud',icon=icon('play-circle'))),
              br()
              ),
              fluidRow(
              column(10,wordcloud2Output('cloud',width='1600px',height='800px'),offset = 1)
              )
              
             ),
             tabPanel('JD items',
                      h2('Comment wordcloud'),
                      fluidRow(
                        column(10,textInput('JDurl',strong('Input the JD items link below 
                                                         (Please ensure that the link contain id.html) '),
                                            defaulturlJD,width='1600px')),
                        
                        column(2,numericInput('JDpages','Number of pages comments to crawl', 5,min=1,max=100))
                        ),
                      
                      fluidRow(
                        column(3,actionButton('JDrun','Click the button to start the web crawl',icon=icon('play-circle'))),
                        column(3,downloadButton('JDdata',label='Download the comments data'))
                        
                      ),
                      br(),
                      fluidRow(
                        column(4,uiOutput('JDstatus'))
                      ),
                      h3('Word cloud summary',align='c'),
                      fluidRow(
                        column(10,textInput('JDstopwords',strong('Input the words to exclude (Please use 、 to seperate the words)'),
                                            value='了、也、好、很、的、不、我、会、说、和、吧',width = '800px')),
                        column(12,actionButton('JDrecloud','Click the button to generate word cloud',icon=icon('play-circle'))),
                        br()
                      ),
                      fluidRow(
                        column(10,wordcloud2Output('JDcloud',width='1600px',height='800px'),offset = 1)
                      )
             )
              
  )
})
                      
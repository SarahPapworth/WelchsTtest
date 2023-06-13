library(shiny)
library(ggplot2)
#This ShinyApp allows you to upload data and learn at Welch's t test.
ui <- fluidPage(titlePanel("Welch's t test"),   # Title of the app
                # build a sidebarPanel
                mainPanel(
                  tabsetPanel(
                    tabPanel("1. Your data", br(),    #The first tab allows you to upload data                        
                             p("This webpage allows you to perform a Welch's t 
                               test. A Welch's t test is used to compare the mean values of two groups."),
                            br(),
                             p(" The null 
                               hypothesis of Welch's t test is that the means of the two groups are the same. To conduct a Welch's t test, you need two variables - the response variable should be numerical, 
                               and the explanatory variable should be categorical, with two groups."),
                             hr(),
                             p("Enter your data in Excel, with the categorical variable in one column, and the numerical variable in other column. Make sure the top line is column names, so you can tell which column is which.") 
                               ,br(), p("When you save the data, use 'Save as' and save the data as either a CSV (Comma delimited) file or a Text (Tab delimited) file. If you have spaces in your column names or category types, a CSV file is better."),
                               br(),p("Choose the file type you used, then click 'Browse...' and a window will open. You can navigate to your file. If you upload the data correctly, you should see it displayed below."),
                             fluidRow(column(5,
                                             selectInput('sep', 'File type:',
                                                         choices=c("","Text (Tab delimited)","CSV (Comma delimited)"),multiple=F) 
                                  ),
                               column(5,
                                      fileInput('file1', 'Choose file',
                                                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                       )),
                             
                             
                             br(),tableOutput(outputId = 'table.output'),br()
                             )
                    ,
                    
                    
                    
                    tabPanel("2. Plot your data", #The second tab allows you to plot it
                             br(),
                             p("Let's look at your data. For Welch's t test, the explanatory variable is categorical, and the response variable is numerical. Select your variables, choose which type of plot you would like, and add informative labels for your axes."),
                             br(),
                             p("When you click 'Descriptive statistics and plot', you'll see a summary table with descriptive statistics, and the plot of your data."),
                             fluidRow(column(4,
                                             selectInput("catvar", "Categorical variable:",
                                                         choices=uiOutput('choose_columns'),multiple=F),
                                             selectInput("numvar", "Numerical variable:",
                                                         choices=uiOutput('choose_columns'),multiple=F)),
                                      column(3, selectInput("plot.type", "Plot type:",
                                                            choices=c("Boxplot","Violin plot"),multiple=F),helpText('Select the variables you want to plot and add informative axis labels.')),
                                      column(5, textInput(inputId = "xlab",
                                                          label = "Explanatory variable axis label"),
                                             textInput(inputId = "ylab",
                                                       label = "Response variable axis label"),
                                             
                                             actionButton("newplot", "Descriptive statistics and plot"))),
                             hr(),
                             tableOutput(outputId = 'summary'),br(),textOutput(outputId='saveplot'),br(),plotOutput(outputId = "coolplot"),br(),br()),
                    tabPanel("3. Testing the assumptions of Welch's t test", br(), #this tab lets you test the assumptions
                             p("In order for Welch's t test to accurately compare the means of the two groups, there are some assumptions
                               about the dataset which should be met.")
                             , br(),
                             p("Assumption 1: The data are randomly sampled from the two groups. To meet this assumption, your experimental design should ensure each possible sampling unit has an equal chance of being selected. If this assumption is not met, there may be biases in your data which affect your ability to conduct a robust statistical test. "),
                             br(),
                             p("Assumption 2: Both samples are normally distributed. If this assumption is not met, you can compare the distributions of your samples using a Mann-Whitney U test, rather than comparing the means using Welch's t test. Below, you can check if your two samples meet the assumption of normality, using Shapiro-Wilk tests and a histogram."),
                             br(),
                             fluidRow(column(4, selectInput("catvar2", "Categorical variable:",
                                                         choices=uiOutput('choose_columns'),multiple=F)),
                                      column(4, selectInput("numvar2", "Numerical variable:",
                                                            choices=uiOutput('choose_columns'),multiple=F)),
                                      column(1,br(),
                                             actionButton("check", "Check for normality"))),
                             hr(),
                             textOutput(outputId = "infocheck1"),tableOutput(outputId = 'shapiro'), textOutput(outputId = "infocheck"),plotOutput(outputId = "histplot"),br()
                             
                             ),
                    tabPanel("4. Analyse your data", #Choose your test and analyse the data
                             br(),
                             p("If your data did meet the assumptions of Welch's t-test, then select 'Welch's t-test' below. If it did not, then select 'Mann-Whitney U test'."),br(),
                             
                             fluidRow(column(6,  br(),br(),selectInput("catvar1", "Categorical variable:",
                                                      choices=uiOutput('choose_columns'),multiple=F),
                                          selectInput("numvar1", "Numerical variable:",
                                                      choices=uiOutput('choose_columns'),multiple=F)),
                                      column(6, helpText('Select the test you wish to perform.'),selectInput("test.type", "Test type:",
                                                      choices=c(" ","Welch's t-test","Mann Whitney U test"),multiple=F),
                                          
                                          actionButton("dotest", "Test"))
                                          
                                         
                             ),
                           textOutput(outputId = "infotext1"),br(),tableOutput(outputId = 't.value'),textOutput(outputId = "infotext2") ,br()  
                             
                    ),
                    tabPanel("5. FAQs", #Some answers to frequently asked questions.
                             br(),
                             h4("Why use a Welch's t-test and not Students t-test?"),
                             p("Student's t-test was developed to compare the means of two groups with the same sample size and variance. However, often the two samples we want to compare do not have the same sample size or variance. To analyse this sort of data, the Welch's t-test (sometimes called the unequal variances t-test) was developed. Welch's t-test gives identical results to a Students t-test when sample sizes and variances are equal, but is more accurate when sample sizes or variances are unequal. Therefore, we should always prefer to use Welch's t-test."),a("You can learn more here",href="https://academic.oup.com/beheco/article/17/4/688/215960"),
                             br(),
                               h4("Why do I reject the null hypothesis when p<0.10 for a Shapiro-Wilk test, but when p<0.05 for all other tests?"),
                             p("There is some inaccuracy in the Shapiro=Wilk test at very small (<20) and very large sample sizes (>5000). This means the calculated p value is slightly inaccurate, but using 0.10 as the critical p value means we keep our probability of making a Type I error low."),a("You can learn more here",href="https://www.jstor.org/stable/2986146"),
                             h4("Can I use Welch's t-test or a Mann-Whitney U test to compare 3 or more groups?"),
                             p("No, you should use a one-way ANOVA or a Kruskal-Wallis test. When you have multiple groups, it might be tempting to compare each group to all the other groups using a number of Welch's t-tests or Mann-whitney U tests. However, every time we do a statistical test, we have a type I error rate of 0.05. If we conduct multiple tests then we compound these errors, and increase our chances of erroneously rejecting the null hypothesis. Therefore, don't use the Welch's t-test or Mann-Whitney U test for analysis when you have more than two groups."),a("You can learn more here",href="https://europepmc.org/articles/pmc3916511"),
                             br(),br()
                    ))))



server <- function(input, output,session) {  
  
  
  session$onSessionEnded(stopApp)
  
  data_set <- reactive({
    req(input$file1)
    inFile <- input$file1
    
    sep<-switch(input$sep,
                      "CSV (Comma delimited)" 	= 	read.csv,
                      "Text (Tab delimited)" 		=	read.table
    )
    
    data.frame(sep(inFile$datapath, header=T))
    
  })
  
  output$table.output <- renderTable({
    
    data_set()
    
  })
  
  
  
  
  observeEvent(input$file1,{
    updateSelectInput(session,
                      inputId = "catvar",
                      choices = names(data_set()))}) 
  
  observeEvent(input$file1,{
    updateSelectInput(session,
                      inputId = "numvar",
                      choices = names(data_set()))}) 
  
  observeEvent(input$file1,{
    updateSelectInput(session,
                      inputId = "catvar1",
                      choices = names(data_set()))}) 
  
  observeEvent(input$file1,{
    updateSelectInput(session,
                      inputId = "numvar1",
                      choices = names(data_set()))}) 
  
  
  observeEvent(input$file1,{
    updateSelectInput(session,
                      inputId = "catvar2",
                      choices = names(data_set()))}) 
  
  observeEvent(input$file1,{
    updateSelectInput(session,
                      inputId = "numvar2",
                      choices = names(data_set()))}) 
  
  
  
  output$choose_columns <- renderUI({
    
    
    # Get the data set with the appropriate name
    
    colnames <- names(data_set())
  })                            
  
  
  
  observeEvent(
    eventExpr = input[["newplot"]],
    handlerExpr = {
      print("PRESSED") 
      output$coolplot <- renderPlot({
        
        plot.type<-switch(input$plot.type,
                          "Boxplot" 	= 	geom_boxplot,
                          "Violin plot" 		=	geom_violin
        )
        
        
        
        g <- ggplot(data_set())+plot.type(aes_string(x=input$catvar, y = input$numvar),trim=FALSE)+ 
          theme_bw()+theme(text=element_text(size=20),axis.title.y=element_text(vjust=5))+ xlab(input$xlab)+ylab(input$ylab)
        print(g)
        
      })
    })
  observeEvent(
    eventExpr = input[["newplot"]],
    handlerExpr = {
      print("PRESSED") 
      output$saveplot <- renderText({
        
        "Inside the box, the plot shows the median, and the interquartile range (IQR, the values between the first and third quantiles). The whiskers extend out from the box to the maximum and minimum values that are within 1.5 x IQR of the extent of the box. Any values which are more than 1.5 x IQR from the box are considered outliers and are plotted individually. If you would like to save the plot, right click on it and select 'Save Image'."
        
      })
    })
    
  observeEvent(
    eventExpr = input[["check"]],
    handlerExpr = {
      print("PRESSED") 
      output$histplot <- renderPlot({
        
        a1<-ggplot(data_set())+geom_histogram(aes_string(x=input$numvar2,color=input$catvar2, fill=input$catvar2), alpha=0.5, position="identity")+scale_color_brewer(palette="Set1")+
          scale_fill_brewer(palette="Set1")+theme(text=element_text(size=18))+ ylab("Number of observations")
          
        print(a1)
        
      })
    })

  
    
  observeEvent(
    eventExpr = input[["check"]],
    handlerExpr = {
      print("PRESSED") 
      output$shapiro<- renderTable({
        shap<-tapply(data_set()[,input$numvar2],as.character(data_set()[,input$catvar2]),shapiro.test)
        tab = matrix(c(names(shap[1]),names(shap[2]),round(shap[[1]]$statistic,digits=3),round(shap[[2]]$statistic,digits=3),round(shap[[1]]$p.value,digits=3),round(shap[[2]]$p.value,digits=3)),nrow=2)
        colnames(tab) = c("group","W", "p")
        rownames(tab) = c("Group 1","Group2")
        tab
      })
      
    })  
  
  observeEvent(
    eventExpr = input[["check"]],
    handlerExpr = {
      print("PRESSED") 
      output$infocheck1<- renderText({
        "Below you can see the results of the Shapiro-Wilk test for each group. The null hypothesis of the Shapiro-Wilk test is that the data is normally distributed, and unlike most statistical tests, we reject the null hypothesis if p<0.10. Therefore, if either of your two groups have a p value which is lower than 0.10, you are unable to conclude that the sample is normally distributed. As one of the assumptions of Wlech's t test is that both samples are normally distributed, you need to conduct an alternate test: the Mann-Whitney U test. If the p value for both tests is greater than 0.10, then you have insufficient evidence to reject the null hypothesis of the Shapiro-Wilk test, and can conclude your data is normally distributed, allowing you to use Welch's t test. Be careful however, as the Shapiro-Wilk test can be misleading when you have very big and very small samples."
  
         })
      
    })  
  
  observeEvent(
    eventExpr = input[["check"]],
    handlerExpr = {
      print("PRESSED") 
      output$infocheck<- renderText({
         
        "You should also always visually inspect your data. When plotted on a histogram, normally distributed data will follow an approximate bell curve, although this can be hard to determine when there are small sample sizes. If you have small a small sample size, it can be best to be cautious and use a Mann-Whitney U test." 
      })
      
    })  
  
  
  observeEvent(
    eventExpr = input[["dotest"]],
    handlerExpr = {
      print("PRESSED") 
      output$infotext1<- renderText({
        
        if(input$test.type=="Welch's t-test")
        {
          paste("The table below shows the difference between the mean ", input$numvar1, " of the group ",levels(data_set()[,input$catvar1])[1]," and the mean ", input$numvar1, " of the group  ", levels(data_set()[,input$catvar1])[2], ", and the 95% confidence interval (CI) of this difference between the means. It also shows the t value calculated from the data, the degrees of freedom, and the probability (p-value) of observing the same or greater t value if the null hypothesis is true." )
          
        }
        else if(input$test.type=="Mann Whitney U test")
        {
          paste("The table below shows the median ", input$numvar1, "  of the ",levels(data_set()[,input$catvar1])[1]," and ",levels(data_set()[,input$catvar1])[2]," groups. It also shows the U value calculated from the data, and the probability (p-value) of observing the same or greater U value if the null hypothesis is true.")
         } 
        else if(input$test.type==" ")
          {
          "Please select a test type."  
        }
        
          
                      })
      })

  observeEvent(
    eventExpr = input[["dotest"]],
    handlerExpr = {
      print("PRESSED") 
      output$infotext2<- renderText({
        
        if(input$test.type=="Welch's t-test")
        {
          paste("Remember that null hypothesis of Welch's t test is that the ", input$numvar1, "  of the group ",levels(data_set()[,input$catvar1])[1]," and the ", input$numvar1, "  of the group ",levels(data_set()[,input$catvar1])[2]," have the same mean. If the p value for the test is smaller than 0.05, then we can reject the null hypothesis that mean ", input$numvar1," of the two groups are the same. If the p value for the test is bigger than 0.05, then we are unable to reject the null hypothesis that the two means are the same.") 
          
        }
        else if(input$test.type=="Mann Whitney U test")
        {
          paste("Remember that the null hypothesis of a Mann-Whitney U test is that the ", input$numvar1, "  of the group ",levels(data_set()[,input$catvar1])[1]," and the ", input$numvar1, "  of the group ",levels(data_set()[,input$catvar1])[2]," have the same distribution. If the distributions do differ, this may be due to a difference in their median, skew or spread - you may be able to see this visually by looking at the histogram on the 'Testing the assumptions of Welch's t test' tab. If the p value for the test is smaller than 0.05, then we can reject the null hypothesis that the distributions of ", input$numvar1," for the two groups are the same. If the p value for the test is bigger than 0.05, then we are unable to reject the null hypothesis that the distributions of the two groups are the same.")
        } 
        else if(input$test.type==" ")
        {
          ""  
        }
        
        
      })
    })
  
  observeEvent(
    eventExpr = input[["dotest"]],
    handlerExpr = {
      print("PRESSED") 
      output$t.value<- renderTable({
        
        if(input$test.type=="Welch's t-test")
        {
          
          
          mod1<-t.test(data_set()[,input$numvar1]~as.character(data_set()[,input$catvar1]),data=data_set(),na.rm=T)
          
          tab = matrix(c(mod1$estimate[1]-mod1$estimate[2],mod1$conf.int[1], mod1$conf.int[2],mod1$parameter,mod1$statistic,mod1$p.value),nrow=1)
          colnames(tab) = c("difference in means","Lower 95% CI", "Lower 95% CI","degrees of freedom","t statistic","p-value")
          rownames(tab) = "Values"
          
        }
        if(input$test.type=="Mann Whitney U test")
        {
          a1<-subset(data_set()[,input$numvar1],subset=data_set()[,input$catvar1]==levels(data_set()[,input$catvar1])[1]) 
          print(a1)
          b1<-subset(data_set()[,input$numvar1],subset=data_set()[,input$catvar1]==levels(data_set()[,input$catvar1])[2]) 
          print(b1)
          mod2<-wilcox.test(a1,b1)
          
          tab = matrix(c(median(a1,na.rm=T),median(b1,na.rm=T),mod2$statistic,mod2$p.value),nrow=1)
          colnames(tab) = c(paste(levels(data_set()[,input$catvar1])[1],"median"),paste(levels(data_set()[,input$catvar1])[2],"median"),"U statistic","p-value")
          rownames(tab) = "Values"
          
        } 
        if(input$test.type==" ")
        {
          tab<-NULL
        }
        tab
      })
    })
  
      
  observeEvent(
    eventExpr = input[["newplot"]],
    handlerExpr = {
      print("PRESSED") 
      output$summary<- renderTable({
        mean1<-tapply(data_set()[,input$numvar],as.character(data_set()[,input$catvar]),mean,na.rm=T)
        n1<-tapply(data_set()[,input$numvar],as.character(data_set()[,input$catvar]),function(x){sum(!is.na(x))})
        min1<-tapply(data_set()[,input$numvar],as.character(data_set()[,input$catvar]),min,na.rm=T)
        med1<-tapply(data_set()[,input$numvar],as.character(data_set()[,input$catvar]),median,na.rm=T)
        max1<-tapply(data_set()[,input$numvar],as.character(data_set()[,input$catvar]),max,na.rm=T)
        sd1<-tapply(data_set()[,input$numvar],as.character(data_set()[,input$catvar]),sd,na.rm=T)
        tab = matrix(c(names(n1[1]),names(n1[2]),n1[1],n1[2],min1[1],min1[2],med1[1],med1[2],max1[1],max1[2],round(mean1[1],digits=2),round(mean1[2],digits=2),round(sd1[1],digits=2),round(sd1[2],digits=2)),nrow=2)
        colnames(tab) = c("group","sample size", "minimum", "median","maximum","mean","standard devation")
        rownames(tab) = c("Group 1","Group2")
        tab
        })
  
    })

}

shinyApp(ui = ui, server = server)

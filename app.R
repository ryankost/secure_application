library(shiny)
library(sodium)
library(shinyBS)
library(shinyjs)
library(dplyr)

# Initial Logic -----------------------------------------------------------

Logged <- FALSE;
LoginPass <- 0; 


# UI Conditional Sections -------------------------------------------------

login <-  div(tags$head(tags$link(rel='stylesheet',type='text/css',href='login.css')),
              tags$h2('Authentication'),
              textInput("userName", "Username"),
              passwordInput("passwd", "Password"),
              br(),actionButton("Login", "Log in"), id='loginbox')
  

loginfail <- div(tags$h2("Authentication"),
                              textInput("userName", "Username"),
                              passwordInput("passwd", "Password"),
                              tags$h5("Username or password incorrect"),
                              br(),actionButton("Login", "Log in"),id='loginfail')


app_main_body <- div(
  fluidPage(
    
    #headers
    tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
    
    
    uiOutput('adminView'),
    
    bsCollapsePanel(
      title = 'User',
      tags$h3('Change Password'),
      passwordInput('currentPassword','Current Password'),
      passwordInput('newPassword','Enter New Password'),
      passwordInput('newPasswordRe','Re-Enter New Password'),
      actionButton('changePassword','Change Password'),
      style = 'default'
    ),
    
    tags$h1('Authentication Successful')
             

    
    
    )
  )


ui <- uiOutput('body')

server <- function(input, output, session) {
   
  USER <<- reactiveValues(Logged = Logged, LoginPass = LoginPass, Permission = NULL)

# Authentication ----------------------------------------------------------

auth_data <- readRDS('authentication.RDS') 
  
          #here's our hash function
          hashFunction <- function(x){
            y <- as.character(x)
            passphrase <- charToRaw(y)
            sodium::scrypt(passphrase,size=64)
          }

          
observe({

  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        username <- isolate(input$userName)
        password <- isolate(input$passwd)

        if (identical(auth_data[which(auth_data$login==input$userName),2][[1]],hashFunction(input$passwd)) == 'TRUE') {
          USER$Logged <<- TRUE
          USER$LoginPass <<- 1
          USER$Permission <<- as.character(auth_data[which(auth_data$login==input$userName),3])
        }
        USER$LoginPass <<- -1
        

      }
    }
  }
})


# Main Body UI Output -----------------------------------------------------

output$body <- renderUI({
  if (USER$Logged == TRUE) {
    app_main_body
  }
  else {
    if(USER$LoginPass >= 0) {
      login
    }
    else {
      loginfail
    }
  }
})



# Admin Level Permissions UI -------------------------------------------------

output$adminView <- renderUI({
  
  userVector <- auth_data %>% filter(login != input$userName) %>% distinct(login) %>% .$login
  
  if(USER$Permission == 'admin'){
  
    bsCollapsePanel(
      title = 'Admin View',
      
      tags$h3('Administrator View'),
      
      fluidRow(
        column(4, tags$h4('Add User'),
               
               textInput('newUser','New Username'),
               selectInput('newUserPriv',' Assign Permissions', choices = c('admin','standard')),
               actionButton('createUser','Add New User')
               
               ),
        column(4, tags$h4('Remove User'),
               
               selectInput('removeUser','Select User to Remove',choices = userVector),
               actionButton('removeUserButton','Remove User')
               
               ),
        column(4, tags$h4('Reset Password'),
               
               selectInput('resetPass','Select User to Reset Password',choices = userVector),
               actionButton('resetPassButton',"Reset User's Password")
               
               )
      ),
      
      style = 'default'
    )
    
      
  }

})



# Adding New User ---------------------------------------------------------

newUserReactive <- reactive({
  
  if(nchar(input$newUser)>0){
    
    data.frame(login=input$newUser,pswd=I(list(hashFunction('hello123'))),perm=input$newUserPriv)
    
  }
  
})


observeEvent(input$createUser,{
  
  if(length(newUserReactive())==0){
    
    js_string <- 'alert("No User Name Provider");'
    session$sendCustomMessage(type='jsCode', list(value = js_string))
    
  } else {
    
    if((newUserReactive()$login %in% auth_data$login)==TRUE){
      
      js_string <- 'alert("User Name Cannot Be Used Twice");'
      session$sendCustomMessage(type='jsCode', list(value = js_string))
      
    } else {
      
      df <- rbind(newUserReactive(),auth_data)
      saveRDS(df,'authentication.RDS')
      
      js_string <- 'alert("User Added. Temporary Password is: hello123 ");'
      session$sendCustomMessage(type='jsCode', list(value = js_string))
      
    }
  }
})


# Remove User -------------------------------------------------------------

removeUserReactive <- reactive({
  
  auth_data[-c(which(auth_data$login == input$removeUser)),]
  
  
})

observeEvent(input$removeUserButton,{
  
  df <- removeUserReactive()
  saveRDS(df,'authentication.RDS')
  
  js_string <- 'alert("User Removed");'
  session$sendCustomMessage(type='jsCode', list(value = js_string))
  
})


# Reset User Password -----------------------------------------------------

resetUserReactive <- reactive({
  
  df <- data.frame(
    login=input$resetPass,
    pswd=I(list(hashFunction('hello123'))),
    perm = as.character(auth_data[which(auth_data$login==input$resetPass),3])
  )
  
  dfMinus <- auth_data[-c(which(auth_data$login == input$resetPass)),]
  
  rbind(dfMinus,df)
  
})

observeEvent(input$resetPassButton,{
  
  df <- resetUserReactive()
  saveRDS(df,'authentication.RDS')
  
  js_string <- 'alert("User Password Reset to hello123");'
  session$sendCustomMessage(type='jsCode', list(value = js_string))
  
})


# User Self-Change Pass ---------------------------------------------------


observeEvent(input$changePassword,{
  
  if((nchar(input$currentPassword)==0)==TRUE | (nchar(input$newPassword)==0)==TRUE | (nchar(input$newPasswordRe)==0)==TRUE){
    
    js_string <- 'alert("Error");'
    session$sendCustomMessage(type='jsCode', list(value = js_string))
    
  } else {
    #does input match current password
    if(identical(hashFunction(input$currentPassword),auth_data[which(auth_data$login == input$userName),2][[1]])==TRUE){
      
      if(input$newPassword == input$newPasswordRe & nchar(input$newPassword)>6){
        
        df <- data.frame(
          login=input$userName,
          pswd=I(list(hashFunction(as.character(input$newPassword)))),
          perm = as.character(auth_data[which(auth_data$login==input$userName),3])
        )
        
        dfMinus <- auth_data[-c(which(auth_data$login == input$userName)),]
        
        df <- rbind(dfMinus,df)
        
        saveRDS(df,'authentication.RDS')
        
        js_string <- 'alert("You Password Has Been Changed");'
        session$sendCustomMessage(type='jsCode', list(value = js_string))
        
      } else {
        js_string <- 'alert("New passwords do not match, or are not atleast 6 characters long");'
        session$sendCustomMessage(type='jsCode', list(value = js_string))
      }
      
    } else {
      js_string <- 'alert("Passwords do not match");'
      session$sendCustomMessage(type='jsCode', list(value = js_string))
    }
    
  }
  
  
})

# Core Server Code Below --------------------------------------------------------

  

}


shinyApp(ui = ui, server = server)


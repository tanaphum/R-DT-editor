#
# code (DT editor) from https://blog.rstudio.com/2018/03/29/dt-0-4/
# by Yihui Xie
#
# concept code (undo) from https://rdrr.io/github/GerkeLab/grkShinyThings/man/undoHistory.html
#
# developed code by Tanaphum Wichaita
# https://github.com/tanaphum

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    x = iris
    x$Date = Sys.time() + seq_len(nrow(x))
    output$x1 = renderDT(x, selection = 'none', editable = TRUE)
    
    proxy = dataTableProxy('x1')
    
    history <- reactiveValues(

            past = NULL,
            current =NULL,
            future = NULL
        
    )
    1
    observeEvent(input$x1_cell_edit, {
        info = input$x1_cell_edit
        i = info$row
        j = info$col
        v = info$value

        if(is.null(history$past)){
        history$future <- NULL
        past <- input$x1_cell_edit
        past$value <-  x[i, j]
        history$past <- past
        history$current <-  input$x1_cell_edit
        }else{
            if(!is.null(history$future)){
                history$future <- NULL
            }
            
            if(history$current[1,1] != input$x1_cell_edit[1,1] || history$current[1,2] != input$x1_cell_edit[1,2]){
                past <- input$x1_cell_edit
                past$value <-  x[i, j]
                history$past <- rbind(history$past,history$current)
                history$past <- rbind(history$past,past)
                history$current <-  input$x1_cell_edit
                
            }else{
            
            history$past <- rbind(history$past,history$current)
            history$current <-  input$x1_cell_edit
            }
        }
        
        
        
        x[i, j] <<- DT::coerceValue(v, x[i, j])
        x[i,length(x)] <- Sys.time()
        replaceData(proxy, x, resetPaging = FALSE)  # important
    })
    
    observeEvent(input$undo, {
            req(!is.null(history$past))
        
            l <- length(history$past[,1]) #col length
            a <- history$past[l,1]
            b <- history$past[l,2]

            if(history$past[l,3] == x[a,b]){
                history$future <- rbind(history$future,history$current,history$past[l,])
                history$current <- history$past[l-1,]
                history$past <- history$past[1:(l-1),]
                l <- length(history$past[,1])
            }else{
            history$future <- rbind(history$future,history$current)
            history$current <- history$past[l,]
            }
            
            
            if(l==1){
                history$past <- NULL
            }else{
            history$past <- history$past[1:(l-1),]
            }

            i = history$current[1,1]
            j = history$current[1,2] 
            v = history$current[1,3]
            x[i, j] <<- DT::coerceValue(v, x[i, j])
            x[i,length(x)] <- Sys.time()
            replaceData(proxy, x, resetPaging = FALSE) 
    })
    
    observeEvent(input$redo, {
        req(!is.null(history$future))
        
        l <- length(history$future[,1]) #col length
        a <- history$future[l,1]
        b <- history$future[l,2]
        if(history$future[l,3] == x[a,b]){
            history$past <- rbind(history$past,history$current,history$future[l,])
            history$current <- history$future[l-1,]
            history$future <- history$future[1:(l-1),]
            l <- length(history$future[,1])
        }else{
            history$past <- rbind(history$past,history$current)
            history$current <- history$future[l,]
        }
        
        
        if(l==1){
            history$future <- NULL
        }else{
            history$future <- history$future[1:(l-1),]
        }
        
        i = history$current[1,1]
        j = history$current[1,2] 
        v = history$current[1,3]
        x[i, j] <<- DT::coerceValue(v, x[i, j])
        x[i,length(x)] <- Sys.time()
        replaceData(proxy, x, resetPaging = FALSE) 
    })
    
    output$past<-renderPrint({
        history$past
     })

    output$current<-renderPrint({
        history$current
    })
    
    output$future<-renderPrint({
        history$future
    })

})

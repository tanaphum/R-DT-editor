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
        str(info)
        i = info$row
        j = info$col
        v = info$value

        if(is.null(history$past)){
        past <- input$x1_cell_edit
        past$value <-  x[i, j]
        history$past <- past
        history$current <-  input$x1_cell_edit
        }else{
            if(history$past[[1]] != input$x1_cell_edit[[1]] || history$past[[1]] != input$x1_cell_edit[[1]]){
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
        
            l <- length(history$past[,1])
            history$future <- rbind(history$future,history$current)
            history$current <- history$past[l,]
            
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
        
        l <- length(history$future[,1])
        history$past <- rbind(history$past,history$current)
        history$current <- history$future[l,]
        
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

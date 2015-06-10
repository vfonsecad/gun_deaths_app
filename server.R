##########################################################
#######   ASMDA PROJECT APP FOR VISUALIZING TEXTUAL DATA RESULTS
##########################################################
library(shiny)
library(googleVis)
library(tm)
library(ggplot2)
library(grid)
library(stringr)


documentos<-data.frame(read.table("docs_ejes.txt"))
documentos.frase<-data.frame(read.table("docs_ejes_phrase.txt"))
row.names(documentos)<-documentos.frase$Artículo
palabras<-data.frame(read.table("palabras_ejes.txt"))
nombres.ejes<-c("Family crimes", "Trials", "Evidences and tests results", "Automobile deaths", "Urban crimes", "Reported cases")
names(documentos)<-nombres.ejes
names(palabras)<-nombres.ejes

word_axis<-round(palabras,2)
art_axis<-round(cbind(documentos, 1:dim(documentos)[1]),2)
names(art_axis)<-c(names(documentos),"ID")


complete_corpus<- Corpus(DirSource("articulos/"))


shinyServer(
  function(input,output){
    #filters    
    data.def<-data.frame(A=c(-6,-4,4,6,4,-4,-6),B=c(0,4,4,0,-4,-4,0))
    data.def1<-data.def[1:6,]/5
    words_print<-reactive({
      axis<-input$them.axis
      id.w<-which(names(word_axis)%in%axis)
      words<-subset(word_axis, select=input$them.axis)
      name.words<-row.names(words)
      id.order<-sort(words[,1], decreasing=TRUE,index.return=TRUE)$ix
      words1<-data.frame(Score=words[id.order[1:20],])
      row.names(words1)<-name.words[id.order[1:20]]
      words_only<-data.frame(Results=row.names(words1))
      return(words1)
    })
    output$Terms_x_axis<-renderTable({words_print()})
    articles_print<-reactive({
      arts<-data.frame(Score=art_axis[,names(art_axis)%in%input$them.axis], ID=factor(art_axis[,7]))
      row.names(arts)<-row.names(art_axis)
      id<-sort(arts$Score,decreasing=TRUE, index.return=TRUE)$ix
      arts1<-arts[id,]
      return(arts1)
    })
    output$Arts_x_axis<-renderTable({articles_print()})
    article_title<-reactive({
      input$goButton
      isolate(articles_print()[articles_print()$ID==(input$artid),])
      })
    output$art_title<-renderTable({article_title()})
    graph<-reactive({
      input$goButton
      x<-isolate(as.vector(t(art_axis[input$artid,1:6])))
      a1<-as.vector(t(x))
      a<-cbind(ifelse(a1>0,a1,0)+1,ifelse(a1>0,a1,0)+1)
      aa<-a*data.def1
      aa<-data.frame(rbind(aa,aa[1,]), scores=round(c(a1,a1[1]),2))
      COLOR<-"#616166"
      COLOR2<-"#1A5930"
      plot.graph<-ggplot(data.def, aes(A,B))+geom_point(size=1)+
        geom_segment(aes(x=data.def1[1,1],y=data.def1[1,2], xend=data.def[1,1], yend=data.def[1,2]), color=COLOR,arrow=arrow(length=unit(0.2,"cm")))+
        geom_segment(aes(x=data.def1[2,1],y=data.def1[2,2], xend=data.def[2,1], yend=data.def[2,2]), color=COLOR,arrow=arrow(length=unit(0.2,"cm")))+
        geom_segment(aes(x=data.def1[3,1],y=data.def1[3,2], xend=data.def[3,1], yend=data.def[3,2]), color=COLOR,arrow=arrow(length=unit(0.2,"cm")))+
        geom_segment(aes(x=data.def1[4,1],y=data.def1[4,2], xend=data.def[4,1], yend=data.def[4,2]), color=COLOR,arrow=arrow(length=unit(0.2,"cm")))+
        geom_segment(aes(x=data.def1[5,1],y=data.def1[5,2], xend=data.def[5,1], yend=data.def[5,2]), color=COLOR,arrow=arrow(length=unit(0.2,"cm")))+
        geom_segment(aes(x=data.def1[6,1],y=data.def1[6,2], xend=data.def[6,1], yend=data.def[6,2]), color=COLOR,arrow=arrow(length=unit(0.2,"cm")))+
        geom_polygon(data=aa, aes(x=A, y=B),fill="#CCCDF8")+
        geom_path(data=data.def, aes(x=A, y=B),fill="#CCCDF8")+
        geom_path(data=aa, aes(x=A, y=B, colour=A*A), size=6,lineend = "round")+
        geom_text(data=aa[1,], aes(x=A, y=B, label=scores),hjust=1.7,vjust=1.6, color=COLOR2)+
        geom_text(data=aa[2,], aes(x=A, y=B, label=scores),hjust=1.9,vjust=-0.7, color=COLOR2)+
        geom_text(data=aa[3,], aes(x=A, y=B, label=scores),hjust=1.2,vjust=-2.6, color=COLOR2)+
        geom_text(data=aa[4,], aes(x=A, y=B, label=scores),hjust=-0.6,vjust=-1.5, color=COLOR2)+
        geom_text(data=aa[5,], aes(x=A, y=B, label=scores),hjust=-0.8,vjust=0.2, color=COLOR2)+
        geom_text(data=aa[6,], aes(x=A, y=B, label=scores),hjust=0.5,vjust=2.8, color=COLOR2)+
        scale_colour_gradient(low = "#1E2680", high = "#1E2680")+
        theme(panel.grid.major = element_line(colour = "white", size=0), 
              panel.background=element_rect(colour="#FAF8F3", fill="#FAF8F3"))+
        xlab("")+ylab("")+scale_x_continuous(breaks=NULL,limits=c(-8,8))+
        scale_y_continuous(breaks=NULL, limits=c(-5,5))+
        geom_text(aes(x=data.def[1,1], y=data.def[1,2],hjust=1, label=str_wrap("Family crimes",width=4)),colour=COLOR, size=6)+
        geom_text(aes(x=data.def[2,1], y=data.def[2,2],hjust=1, vjust=-1,label=str_wrap("Trials", width=4)),colour=COLOR, size=6)+
        geom_text(aes(x=data.def[3,1], y=data.def[3,2],hjust=0, vjust=-1,label=str_wrap("Evidences",width=6)),colour=COLOR, size=6)+
        geom_text(aes(x=data.def[4,1], y=data.def[4,2],hjust=0, label=str_wrap("Automobile deaths",width=4)),colour=COLOR, size=6)+
        geom_text(aes(x=data.def[5,1], y=data.def[5,2],hjust=-0.5, vjust=1.5,label=str_wrap("Urban crimes",width=4)),colour=COLOR, size=6)+
        geom_text(aes(x=data.def[6,1], y=data.def[6,2],hjust=1, vjust=1.5,label=str_wrap("Reported cases",width=4)),colour=COLOR, size=6)+
        guides(legend=FALSE, colour=FALSE, fill=FALSE, size=FALSE)
      return(plot.graph)      
    })
    output$proj_graph<-renderPlot({graph()},width = 1000, height = 800)
    output$text<-renderPrint({
      input$goButton
      isolate(complete_corpus[[input$artid]])
      })
  })#shinyServer
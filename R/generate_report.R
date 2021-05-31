generate_report <-
function(chg){
  ret <- returns(chg)
  yaml_header <- "
---
title: 'Strategy Report'
output: 
  html_document
fontsize: 12pt
---"
  text <- "

```{r, fig.width=8, fig.height=4, echo=FALSE}
ret_dd_plot(ret)
yret <- yearly_return(chg)
tab <- create_table(chg, ret)
den <- density_plot(chg)
gridExtra::grid.arrange(
  grobs = list(yret, tab, den),
  layout_matrix = rbind(c(1, 2),
                        c(3, 2))
)
create_heatmap(chg)
```{r, echo=TRUE}
# Thank you <3
      "
  rmd_file_name <- "temp.Rmd"
  
  content <- paste0(yaml_header,
                    "\n", 
                    "\n", 
                    text)
  
  write(content, rmd_file_name)
  
  rmarkdown::render(rmd_file_name)
  #utils::browseURL(paste0("file://", utils::URLencode(gsub("Rmd$", "html", rmd_file_name))))
}
\name{generate_report}
\alias{generate_report}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to create a report summarizing all metrics and analytics.
}
\description{Produces a report where all computations, tables and plots are displayed.
}
\usage{
generate_report(chg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chg}{Percentage changes
}
}
\details{Currently no additional details.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Jan Gobeli & Tuomas Ahonen
}
\note{No further notes.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (chg) 
{
    ret <- returns(chg)
    text <- "\n  ---
    \n  title: 'Strategy Report'
    \n  output: \n    
    html_document\n  
    fontsize: 12pt\n  ---\n  \n  
    ```{r, fig.width=8, fig.height=4, echo=FALSE}\n  
    ret_dd_plot(ret)\n  yret <- yearly_return(chg)\n  
    tab <- create_table(chg, ret)\n  den <- density_plot(chg)\n  
    grid.arrange(\n    grobs = list(yret, tab, den),\n    
    layout_matrix = rbind(c(1, 2),\n                          
    c(3, 2))\n  )\n  create_heatmap(chg)\n  
    ```{r, echo=TRUE}\n  
    # Thank you <3\n  "
    rmd_file_name <- "temp.Rmd"
    content <- paste0("\n", text)
    write(content, rmd_file_name)
    rmarkdown::render(rmd_file_name)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

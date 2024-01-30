Custom template for New Graph Environment Ltd. reporting

see `scripts/run.R`

Track version changes in [`NEWS.md`]('NEWS.md`)


Adapted from a minimal example of a book based on R Markdown and **bookdown** (https://github.com/rstudio/bookdown). Please see the page "[Get Started](https://bookdown.org/yihui/bookdown/get-started.html)" at https://bookdown.org/yihui/bookdown/ for how to compile this example into HTML. More detailed instructions are available here https://bookdown.org/yihui/bookdown/build-the-book.html.

So if we want to use this repo to update specific files in existing repos generated from the template we need to can do the following from the production report.  NEED TO TEST A BUNCH.  See https://stackoverflow.com/questions/24815952/git-pull-from-another-repository:

    git remote add upstream https://github.com/NewGraphEnvironment/mybookdown-template.git
    git config remote.upstream.pushurl "maybe dont push to the template from here bud"
    git fetch upstream
    git checkout upstream/master -- path/to/file
    
    
A better option is likely to use submodules http://blog.joncairns.com/2011/10/how-to-use-git-submodules/ but we will look into that next time

In order to avoid commit huge files run this every once and a while https://stackoverflow.com/questions/4035779/gitignore-by-file-size
https://stackoverflow.com/questions/37768376/remove-duplicate-lines-and-overwrite-file-in-same-command

    find . -size +50M | sed 's|^\./||g' >> .gitignore; awk '!seen[$0]++' .gitignore | sponge .gitignore
    
    
This is a common move to deal with repeated headers in pagedown knitr table outputs when the page breaks.  If we don't have an extra `<br>`

`r if(gitbook_on){knitr::asis_output("<br>")} else knitr::asis_output("\\pagebreak<br>")`
    

   

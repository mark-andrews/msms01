docker run --rm -p 8787:8787 -e PASSWORD=foo -e ROOT=true -v $(dirname $(pwd)):/home/rstudio/msms01 msms01:latest 

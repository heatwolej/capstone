directory = "C:/dev/r/capstone/SwiftKey/en_US"
newsfile <- "en_US.news.txt"
filePath = paste0(directory, '/', newsfile)
cnx = file(filePath, open = "r")
message(paste('File', filePath, 'opened for reading'))
linesChunkSize = 100000
totalLinesCount = 0
while (TRUE) {
  lines = readLines(cnx, n = linesChunkSize, encoding = 'UTF-8')  
  linesCount = length(lines)    
  totalLinesCount = totalLinesCount + linesCount
  if (linesCount == 0 ) {
    message("Reached EOF")
    break
  }    
  message(paste(linesCount, 'lines read. Total thus far:', totalLinesCount))
}
close(cnx)

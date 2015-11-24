
------------------------------------------------- USAGE -------------------------------------------------

-- Creating Database file from logfile
    
     cd to TraceFileHandler directory
     cd TraceFileHandler
     runhaskell RunTraceFile2DB.hs -l dummyLogFile.txt -d test1.db
     Above command will parse logfile (dummyLogFile.txt) and insert rows into database file (test1.db)


-- Running TraceWebViewer --

    cd to directory where this Readme.txt or TraceWebViewer.hs is present.
    runhaskell TraceWebViewer.hs 


-- Configuring view

    Open file TraceDBConfig.hs
    You can configure :
        databaseFileName : sqlite database file which stores log/trace rows.
        defaultRowsViewSize : Size of html-view which displays list of rows.
        defaultPagesViewSize : Size of html-view which displays list of pages.
        defaultNumberOfRowsPerPage : Number of rows displayed in html-view which displays list of rows.


-- Filter Query

    Web view has filter input field. Here we will discuss the query language.

-- Query example

    "$level = I" : this will cause result to contains log with level = Informational.
    "$l=E|W"     : $l is synonym of $level. 
        This will filter logs with level = Error or Warning.

-- "$type = http" or "$t = http". 

    This will filter logs whose 'type' field contains string "http"

---- "$message = certificate" or "$m = certificate". 
    
    This will filter logs whose 'message' field contains string "message"

---- Multiple filters example:
    
    $level = I, $type=http, $message=certificate
    Above query is 'AND' of level filter, type filter and message filter.
    Please enter above text in filter input box to see filtered result.

------------------------------------------------- USAGE -------------------------------------------------

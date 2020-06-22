# edgar

Using the `edgar` package to pull in daily Form 4 disclosures made to the SEC. In order to speed the process and limit duplication, the `getFilings()` function has been limited to only pull in the filings for a specific date. 

Filings are made every non-holiday Monday through Friday. 

Data pulled: 
- CIK number
- filing_date
- trade_date
- stock_ticker symbol
- company_name
- insider_name
- insider_title (officer, director, 10% owner, other)
- trade_type (sale, purchase, grant, etc)
- price
- quantity traded
- % owned
- value (cost of transaction)
- type (A or D)
- industry (based on SIC codes)
- division (based on SIC codes)
- access_number (filing number with SEC)

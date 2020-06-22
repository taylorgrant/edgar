# edgar

Using the `edgar` package to pull in daily Form 4 disclosures made to the SEC. In order to speed the process and limit duplication, the `getFilings()` function has been limited to only pull in the filings for a specific date. 

Filings are made every non-holiday Monday through Friday. 

### Data pulled: 
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

### Sample Output

|cik     |filing_date         |trade_date |stock_ticker |company_name                 |insider_name    |insider_title |trade_type | price| quantity|     owned|     value|type |sic  | delta_owned|industry               |division                            |access_number        |
|:-------|:-------------------|:----------|:------------|:----------------------------|:---------------|:-------------|:----------|-----:|--------:|---------:|---------:|:----|:----|-----------:|:----------------------|:-----------------------------------|:--------------------|
|1324410 |2017-07-27 16:38:04 |2017-07-25 |GBNK         |Guaranty Bancorp             |Goss Cathy P    |EVP, CCO      |S - Sale   | 26.82|    -3793|  4225.392| -101728.3|D    |6022 |  -0.4730375|State Commercial Banks |Finance, Insurance, And Real Estate |0001179110-17-010737 |
|1231996 |2019-11-26 09:43:50 |2019-11-22 |EFSI         |EAGLE FINANCIAL SERVICES INC |GILPIN THOMAS T |Director      |S - Sale   | 31.50|      -13| 19607.000|    -409.5|D    |6022 |  -0.0006626|State Commercial Banks |Finance, Insurance, And Real Estate |0000880641-19-000088 |
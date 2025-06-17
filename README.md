# Currency rates
 Currency rates wrappers for crypto coins and valutes
 The unit `cryptocompare.pas`: crypto currencies rates via CryptoCompare.com API.
 The unit `cbrvalutes.pas`: valutes rates via cbr.ru API (The Central Bank of the Russian Federation rates).
 
 Access rates via the `_cbrvalutes` and `_CryptoCompare` objects. Data updates are thread-safe. You don't have to be afraid to frequently access objects, since requests are cached in memory and access is carried out as necessary. There is an example of using it in the repository

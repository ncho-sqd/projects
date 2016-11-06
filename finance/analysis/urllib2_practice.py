import urllib2

SITE = "http://www.cboe.com/products/snp500.aspx"

response = urllib2.urlopen(SITE)
print response.info()
print response.read()
reponse.close()
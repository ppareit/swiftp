from mod_python import apache
from mod_python import util
from log import log
import myutil
#import error_page

import xml.dom.minidom

# TODO: remove debug statements to unclutter Apache logs

def handler(req):
    req.content_type = "text/html"
    gchk_response = None
    
    formdata = util.FieldStorage(req)
    
    android_id = formdata['android_id']
    price = formdata['price']
    transferamt = myutil.form_price_to_transfer_amt(price)
    if transferamt == None:
        # The incoming form price was not one of the valid choices
        log(req, "Invalid price selection")
        req.write(error_page.get())
        return apache.OK
    log(req, "0")
    description = myutil.transfer_amount_to_description(transferamt)
    log(req, "1")
    req.write("android_id " + android_id + " wants to buy " + price \
              + " described as \"" + description + "\"\n")
    log(req, "2")
    postdata = \
"""<?xml version="1.0" encoding="UTF-8"?>

<checkout-shopping-cart xmlns="http://checkout.google.com/schema/2">
  <shopping-cart>
    <items>
      <item>
        <item-name>SwiFTP transfer credit - %(description)s</item-name>
        <item-description>For android_id=%(android_id)s, increase SwiFTP proxy transfer quota by %(description)s</item-description>
        <unit-price currency="USD">%(price)s.00</unit-price>
        <quantity>1</quantity>
      </item>
    </items>
  </shopping-cart>
</checkout-shopping-cart>
""" % {'android_id': android_id, 'transferamt' : transferamt, 'description' : description, 'price' : price}

#    This code is now factored out into the myutil module
#    gchk_url = myutil.get_gchk_url()
#    gchk_req = urllib2.Request(gchk_url)
#    (merch_id, merch_key) = myutil.get_gchk_creds()
#    auth_header = "Basic " + base64.b64encode(merch_id + ':' + merch_key)
#    gchk_req.add_header('Authorization', auth_header)
#    gchk_req.add_header('Content-Type', 'application/xml')
#    gchk_req.add_header('Accept', 'application/xml')
#    log(req, "Sending checkout API req")
#    gchk_response = urllib2.urlopen(gchk_req, postdata).read()
#    req.write("Got response: <br>\n")
#    req.write(gchk_response)
#    req.write("<br>\n")
#    dom = xml.dom.minidom.parseString(gchk_response)

    # Make the web service call and return a minidom object from the received XML
    #url = myutil.get_place_order_url()
    dom = myutil.gchk_call(req, postdata)

    #redir_url = dom.getElementsByTagName("redirect-url")[0].childNodes[0].data
    redir_url = myutil.extract_node(dom, 'redirect-url')
    if not (redir_url[0:5] == 'https'):
        # We didn't get a success response from Google Checkout
        log(req, "Bad response from Google Checkout: " + gchk_response)
        return apache.HTTP_INTERNAL_SERVER_ERROR  # code 500
    log(req, "Formatting")
    req.write("Got URL: <a href=\"%(url)s\">%(url)s</a>" % {'url' : redir_url}) 
        
    return apache.OK



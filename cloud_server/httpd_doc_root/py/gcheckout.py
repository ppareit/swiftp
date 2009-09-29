from mod_python import apache
from log import log
import xml.dom.minidom
import re
import myutil
import string
import json
import socket


def do_db_work(req, py_obj):
    """Open a connection to the Erlang server that handles our database, write the given
    object in JSON format, and return the server's response. The req parameter is a
    mod_python Apache HTTP request object, so we can do logging."""
    json_text = json.dumps(py_obj)
    sock = socket.create_connection(('localhost', 2001))
    sock.send(json_text)
    response = sock.recv(8192)
    sock.close()
    response = json.loads(response)
    if len(response) == 0:
        return True
    else:
        log(req, "Failure response from Erlang server: " + str(response))
        return False

def new_order_notification(req, dom):    
    """Called iff the incoming XML request is a 'new-order-notification'"""    
    order_number = myutil.extract_node(dom, "google-order-number")
    buyer_id = myutil.extract_node(dom, "buyer-id")
    order_total = myutil.extract_node(dom, "order-total")
    timestamp = myutil.extract_node(dom, "timestamp")
    
    # Translate the dollar value of the user's order into GB's of transfer credit,
    # or -1 for infinite
    transfer_credit = myutil.get_price_value_map()[int(float(order_total))]
    
    # The item description in the shopping cart will contain a substring 
    # like "android_id=123456789" that we must extract.
    # Something could throw an exception in this section with bad input, which is fine.
    description_text = myutil.extract_node(dom, ["shopping-cart", "items", "item", \
                                          "item-description"])
    log(req, "Description is: " + description_text)
    re_pattern = re.compile('android_id=([a-zA-Z0-9]+),')
    re_match = re_pattern.search(description_text)
    android_id = re_match.group(1)

    log(req, "Order number is: " + order_number)
    log(req, "Buyer ID is: " + buyer_id)
    log(req, "Android ID is: " + android_id)
    log(req, "Order total is: " + order_total)
    log(req, "Timestamp is: " + timestamp)
    log(req, "Transfer credit: " + str(transfer_credit))
    
    req.content_type = "application/xml"

    if order_number is None or buyer_id is None or android_id is None \
       or order_total is None or timestamp is None or transfer_credit is None:
        req.write("Internal error")
        return apache.HTTP_INTERNAL_SERVER_ERROR

    response = do_db_work(req, \
                          {'event' : 'new-order-notification', \
                           'google-order-number' : order_number, \
                           'android_id' : android_id, \
                           'order-total' : order_total, \
                           'timestamp' : timestamp, \
                           'buyer-id' : buyer_id, \
                           'transfer-credit' : transfer_credit})
    if response == True:
        # On success, we will have received an empty JSON object
        log(req, "New order notification success")
        return apache.OK
    else:
        log(req, "New order notification failed")
        return apache.HTTP_INTERNAL_SERVER_ERROR

def order_state_change_notification(req, dom):
    """Called iff the incoming XML request is an 'order-state-change-notification'"""
    order_num = myutil.extract_node(dom, 'google-order-number')
    order_state = myutil.extract_node(dom, 'new-financial-order-state')
    timestamp = myutil.extract_node(dom, 'timestamp')
    
    response = do_db_work(req, {'event' : 'order-state-change-notification', \
                                'google-order-number' : order_num, \
                                'new-financial-order-state' : order_state, \
                                'timestamp' : timestamp})
                                
    # The server will return an empty JSON object on success
    if response == True:
        log(req, "Order state change successful, order " + str(order_num) + " to state " + str(order_state))
        return apache.OK
    else:
        log(req, "Order state change failed, order " + str(order_num) + " state " + str(order_state))
        return apache.HTTP_INTERNAL_SERVER_ERROR
        
def charge_amount_notification(req, dom):
    """Called iff the incoming XML request is a 'charge-amount-notification'"""
    order_num = myutil.extract_node(dom, 'google-order-number')
    latest_amt = myutil.extract_node(dom, 'latest-charge-amount')
    total_amt = myutil.extract_node(dom, 'total-charge-amount')
    timestamp = myutil.extract_node(dom, 'timestamp')
    
    response = do_db_work(req, {'event': 'charge-amount-notification', \
                                'google-order-number' : order_num, \
                                'latest-charge-amount' : latest_amt, \
                                'total-charge-amount' : total_amt, \
                                'timestamp' : timestamp })
    if response == True:
        log(req, "Charge amount successful, order " + str(order_num) + " amount " + str(total_amt))
        
        log(req, "Sending ship notification for order " + str(order_num))
        postdata =  \
"""<?xml version="1.0" encoding="UTF-8"?>

<deliver-order xmlns="http://checkout.google.com/schema/2"
    google-order-number="%(order_num)s">
</deliver-order>
""" % {'order_num' : order_num}
        log(req, "Postdata is: " + str(postdata))
        # Make the web service call and return a minidom object from the received XML
        dom = myutil.gchk_call(req, postdata)
        log(req, "deliver-order response was " + dom.toxml())
        return apache.OK
    else:
        log(req, "Charge amount call failed, order " + str(order_num) + " amount " + str(total_amt))
        return apache.HTTP_INTERNAL_SERVER_ERROR
    
def handler(req):
    if not req.is_https():
        req.content_type = "text/plain"
        req.write("Must use HTTPS!")
        return apache.OK
    log(req, "gcheckout running, parsing")

    req_body = req.read()
    #log(req, "request body is: " + req_body)
    dom = xml.dom.minidom.parseString(req_body)

    req_type = dom.childNodes[0].nodeName
    if req_type == 'new-order-notification':
        ret_val = new_order_notification(req, dom)
    elif req_type == 'order-state-change-notification':
        ret_val = order_state_change_notification(req, dom)
    elif req_type == 'charge-amount-notification':
        ret_val = charge_amount_notification(req, dom)
    elif req_type == 'risk-information-notification':
        # We don't care about risk information, just return OK
        log(req, "Ignoring incoming risk-information-notification")
        ret_val = apache.OK
    else:
        log(req, "Unrecognized root node type: " + req_type)
        ret_val = apache.HTTP_INTERNAL_SERVER_ERROR
        
    #return ret_val
    return apache.OK


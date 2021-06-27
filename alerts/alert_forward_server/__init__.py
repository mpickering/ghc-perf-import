"""
A simple server which receieves and forwards POST requests with an authentication
header
Usage::
    ./server.py [<port>]
"""
from http.server import BaseHTTPRequestHandler, HTTPServer
import logging

from sys import argv
import requests

from urllib.parse import urlencode
from urllib.request import Request, urlopen

class S(BaseHTTPRequestHandler):
    def _set_response(self):
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()

    def do_POST(self):
        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length)
        logging.info("POST request,\nPath: %s\nHeaders:\n%s\n\nBody:\n%s\n",
                str(self.path), str(self.headers), post_data.decode('utf-8'))

        auth_header = {'Content-Type': 'application/json'
                      , 'Authorization': f'Bearer {authToken}' }
        r = requests.post(endpoint, data=post_data, headers=auth_header)
        logging.info("Resp " + str(r.status_code))
        self._set_response()


def run(server_class=HTTPServer, handler_class=S):
    logging.basicConfig(level=logging.INFO)
    server_address = ('', port)
    httpd = server_class(server_address, handler_class)
    logging.info('Starting httpd...\n')
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    httpd.server_close()
    logging.info('Stopping httpd...\n')

def main():
    global port, authToken, endpoint
    port = int(argv[3])
    authToken = argv[2]
    endpoint = argv[1]
    run()

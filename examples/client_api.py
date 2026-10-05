#!/usr/bin/env python3
"""Independent stdlib client: python3 examples/client_api.py --home DIR health.

POST example:
  python3 examples/client_api.py --home DIR command request-1 @request.json
A receipt is polled; an unknown outcome is never automatically resubmitted.
"""
import argparse
import json
import pathlib
import sys
import time
import urllib.request

class Client:
    def __init__(self, home):
        state = pathlib.Path(home) / '.monty'
        self.url = json.loads((state / 'client-service.json').read_text())['url']
        self.token = (state / 'client-service-token').read_text().strip()

    def request(self, path, data=None, headers=None):
        h = {'Authorization': 'Bearer ' + self.token, **(headers or {})}
        if data is not None:
            h['Content-Type'] = 'application/json'
        req = urllib.request.Request(self.url + '/v1/' + path,
                                     data=None if data is None else json.dumps(data).encode(), headers=h)
        with urllib.request.urlopen(req, timeout=40) as response:
            return json.load(response)

    def history(self, cid):
        offset = 0
        result = None
        while True:
            page = self.request('conversations/' + cid + '/history?offset=' + str(offset))
            if result is None:
                result = page
            else:
                for session, more in zip(result['sessions'], page['sessions']):
                    session['records'].extend(more['records'])
                    session['next_offset'] = more['next_offset']
            if all(session['next_offset'] is None for session in page['sessions']):
                return result
            offset += 8

    def command(self, request_id, data):
        self.request('commands/' + request_id, data)
        for _ in range(400):
            receipt = self.request('commands/' + request_id)
            if receipt['status'] not in ('accepted', 'running'):
                return receipt
            time.sleep(.1)
        raise TimeoutError('Inspect receipt ' + request_id + '; do not issue a new ID automatically')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--home', required=True)
    parser.add_argument('operation')
    parser.add_argument('args', nargs='*')
    args = parser.parse_args()
    client = Client(args.home)
    if args.operation == 'command':
        request_id, payload = args.args
        payload = pathlib.Path(payload[1:]).read_text() if payload.startswith('@') else payload
        result = client.command(request_id, json.loads(payload))
    else:
        result = client.request(args.operation)
    print(json.dumps(result, indent=2))

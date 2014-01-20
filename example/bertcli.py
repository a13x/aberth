#!/usr/bin/env python

"""
    Simple client in Python calling example aberth service
"""

import bertrpc
service = bertrpc.Service('localhost', 10001)
response = service.request('call').example.adder(203,302)
assert response == 505
response = service.request('call').dictoid.to_dict("aberth", "is", "great")
print response

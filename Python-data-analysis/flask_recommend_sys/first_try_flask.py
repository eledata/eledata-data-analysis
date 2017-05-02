#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 2016年11月24日

@author: hmy
'''
from flask import Flask

app = Flask(__name__)

@app.route('/hello_flask')

def hello_flask():
    return "hello flask"

if __name__ == '__main__':
    app.run()
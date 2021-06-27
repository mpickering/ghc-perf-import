#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup

setup(
    name='alert-forward-server',
    version='0.1',
    packages=['alert_forward_server'],
    install_requires=['requests'],
    entry_points = {
        'console_scripts': [
            'alert-forward-server=alert_forward_server:main'
        ]
    }
)



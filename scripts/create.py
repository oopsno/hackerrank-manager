#!/usr/bin/env python3

"""
Why bother?
"""

import requests
import json
import pyquery
import os
import sys

here = os.path.abspath(__file__)
hrank = os.path.abspath(os.path.join(here, '../..'))


def parse_slug(slug: str) -> str:
    words = slug.split('-')
    return ''.join(map(str.title, words))


def get(name):
    url = "https://www.hackerrank.com/rest/contests/master/challenges/{}".format(name)
    try:
        res = requests.get(url)
        model = json.loads(res.content.decode())['model']
        html_body = model['body_html']
        pq = pyquery.PyQuery(html_body)
        codes = [code.text for code in pq('code')]
        if len(codes > 2):
            si, so = codes[-2:]
        else:
            si, so = codes
        track = model['track']
        slugs = [track['track_slug'], track['slug'], model['slug']]
        return si, so, slugs
    except Exception as e:
        print(e)
        sys.exc_info()[2].print_tb()
        print('Cannot create {}'.format(name))
        exit(-1)


def render_utest(module_name, si, so):
    usi = "\"{}\"".format(repr(si)[1:-1])
    uso = "\"{}\"".format(repr(so)[1:-1])
    return """module {}.UnitTest where

import Test.Hspec
import Test.QuickCheck

import {}.Solution as S

sampleInput :: String
sampleInput = {}

sampleOutput :: String
sampleOutput = {}

main :: IO ()
main = putStrLn "No available test cases"

    """.format(module_name, module_name, usi, uso)


def render_solution(module_name):
    return """module {}.Solution where

import Control.Monad
import Control.Applicative

main :: IO ()
main = putStrLn "No available implementation"

    """.format(module_name)


def create(name):
    si, so, slugs = get(name)
    words = [parse_slug(slug) for slug in slugs]
    basedir = os.path.join(*[hrank, 'src', 'HRank'], *words)
    module_name = '.'.join(['HRank'] + words)
    sol_path = os.path.join(basedir, 'Solution.hs')
    utt_path = os.path.join(basedir, 'UnitTest.hs')
    try:
        os.makedirs(basedir)
        with open(sol_path, 'w') as sol:
            sol.write(render_solution(module_name))
        with open(utt_path, 'w') as utt:
            utt.write(render_utest(module_name, si, so))
    except Exception as e:
        print(e)
        print("Get {}, but failed to write local files".format(module_name))
    else:
        print('added {} to {}'.format(module_name, os.path.relpath(basedir)))

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("""
        Usage {} [name]
        """.format(sys.argv[0]))
        exit(0)
    else:
        create(sys.argv[1])


        

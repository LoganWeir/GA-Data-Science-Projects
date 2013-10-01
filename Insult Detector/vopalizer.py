#!/usr/bin/env python

# Specific to adzuna salary data processing.
# Usage:
# (input from stdin, output to stdout)
# Converts to vowpal wabbit format, target is log of salary.
# (Inspiration from FastML.)

import csv
import fileinput
import re
import math

def tokenize(text):
  # Remove apostrophes (inside words, etc.)
  text = text.replace("'", "")
  # Replace any runs of not-digit-letter-or-underscore with a space
  text = re.sub(r"\W+", " ", text)
  text = text.lower()
  text = text.split()
  words = []
  # Only keep one occurence of a word
  for word in text:
    if word in words:
      continue
    words.append(word)
  words = " ".join(words)
  return words

def labelize(text):
  text = text.replace("'", "")
  text = re.sub(r"\W+", "_", text)
  text = text.lower()
  return text

reader = csv.DictReader(fileinput.input())

for line in reader:
  vwline = str(math.log(float(line.get('SalaryNormalized', 1))))
  for field in ['Title', 'FullDescription', 'LocationRaw',
                'Company', 'Category', 'SourceName']:
    vwline = vwline + ' |' + field + ' ' + tokenize(line[field])
  for field in ['loc2', 'loc3', 'loc4', 'loc5', 'loc6', 'loc7',
                'ContractType', 'ContractTime']:
    vwline = vwline + ' |' + field + ' ' + labelize(line[field])
  print vwline
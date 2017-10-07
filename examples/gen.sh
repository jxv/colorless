#! /bin/sh

# HelloWorld
colorless -l haskell -s HelloWorldPublic/specs.json -m HelloWorld -n HelloWorld -d ./HelloWorldPublic/haskell-server -e server -a scotty
colorless -l haskell -s HelloWorldPublic/specs.json -m HelloWorld -n HelloWorld -d ./HelloWorldPublic/haskell-client -e client -a http-client
colorless -l javascript -s HelloWorldPublic/specs.json -n helloWorld -d ./HelloWorldPublic/javascript-client -e client

# HelloWorl with public specs
colorless -l haskell -s HelloWorld/specs -m HelloWorld -n HelloWorld -d ./HelloWorld/haskell-server -e server -a scotty
colorless -l haskell -s HelloWorld/specs -m HelloWorld -n HelloWorld -d ./HelloWorld/haskell-client -e client -a http-client
colorless -l javascript -s HelloWorld/specs -n helloWorld -d ./HelloWorld/javascript-client -e client

# Phonebook
colorless -l haskell -s Phonebook/specs -m Phonebook -n Phonebook -d ./Phonebook/haskell-server -e server -a scotty
colorless -l haskell -s Phonebook/specs -m Phonebook -n Phonebook -d ./Phonebook/haskell-client -e client -a http-client
colorless -l javascript -s Phonebook/specs -n phonebook -d ./Phonebook/javascript-client -e client

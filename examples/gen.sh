#! /bin/sh

# HelloWorld
fluid -l haskell -s HelloWorld/specs -m HelloWorld -n HelloWorld -d ./HelloWorld/haskell-server -e server -a scotty
fluid -l haskell -s HelloWorld/specs -m HelloWorld -n HelloWorld -d ./HelloWorld/haskell-client -e client -a http-client
fluid -l javascript -s HelloWorld/specs -n helloWorld -m HelloWorld -d ./HelloWorld/javascript-client -e client

# HelloWorld with public specs
fluid -l haskell -s HelloWorldPublic/specs.json -m HelloWorld -n HelloWorld -d ./HelloWorldPublic/haskell-server -e server -a scotty
fluid -l haskell -s HelloWorldPublic/specs.json -m HelloWorld -n HelloWorld -d ./HelloWorldPublic/haskell-client -e client -a http-client
fluid -l javascript -s HelloWorldPublic/specs.json -m HelloWorld -n helloWorld -d ./HelloWorldPublic/javascript-client -e client

# Phonebook
fluid -l haskell -s Phonebook/specs -m Phonebook -n Phonebook -d ./Phonebook/haskell-server -e server -a scotty
fluid -l haskell -s Phonebook/specs -m Phonebook -n Phonebook -d ./Phonebook/haskell-client -e client -a http-client
fluid -l javascript -s Phonebook/specs -m Phonebook -n phonebook -d ./Phonebook/javascript-client -e client

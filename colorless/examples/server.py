from flask import Flask, request
from flask_restful import Resource, Api

# json

def to_color(json):
    ty = json['subtype']
    if ty == 'Red':
        return Color.Red()
    elif ty == 'Blue':
        return Color.Blue()
    elif ty == 'Yellow':
        return Color.Yellow()
    elif ty == 'Green':
        return Color.Green()

def from_color(color):
    if color.subtype == 'Red':
        return {'subtype': 'Red'}
    elif color.subtype == 'Blue':
        return {'subtype': 'Blue'}
    elif color.subtype == 'Green':
        return {'subtype': 'Green'}
    elif color.subtype == 'Yellow':
        return {'subtype': 'Yellow'}

# generated sum type

class Color():
    def __init__(self, subtype, args):
        self.subtype = subtype
        self.args = args
    def Red():
        return Color('Red', None)
    def Blue():
        return Color('Blue', None)
    def Yellow():
        return Color('Yellow', None)
    def Green():
        return Color('Green', None)

# generated service

class Service(Resource):
    def __init__(self, domain):
        self.domain = domain

    def post(self):
        body = request.get_json()
        # meta = request.get_headers()

        fn = body.get('fn')
        args = body.get('args', {})

        if fn == 'reverse':
            return str(self.domain.reverse(a=str(args['a'])))
        elif fn == 'age':
            return self.domain.age(**args)
        elif fn == 'different':
            return self.domain.different(**args)
        elif fn == 'circleArea':
            return self.domain.circleArea(**args)
        elif fn == 'enemy':
            return from_color(self.domain.enemy(you=to_color(args['you'])))
        elif fn == 'foo':
            return self.domain.foo(**args)
        return {}

# generated domain interface

class Domain():
    def reverse(self, a):
        pass
    def age(self, id_):
        pass
    def different(self, a, b):
        pass
    def circleArea(self, circle):
        pass
    def enemy(self, you):
        pass
    def foo(self, either):
        pass

# user's domain implementation

class DomainImpl(Domain):
    def reverse(self, a):
        return a[::-1]
    def age(self, id_):
        return 80
    def different(self, a, b):
        return {}
    def circleArea(self, circle):
        return {}
    def enemy(self, you):
        return {
            'Blue': Color.Red(),
            'Red': Color.Blue(),
            'Yellow': Color.Green(),
            'Green': Color.Yellow(),
        }[you.subtype]
    def foo(self, either):
        return {}

# generated server runner

def run_server():
    app = Flask(__name__)
    api = Api(app)
    api.add_resource(Service, '/', resource_class_kwargs={'domain': DomainImpl()})
    app.run(debug=True)

if __name__ == '__main__':
    pass
    run_server()

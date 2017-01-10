from flask import Flask, request
from flask_restful import Resource, Api

# json

def to_color(json):
    ty = json['ty']
    return (ty,)

def from_color(color):
    if color[0] == 'Red':
        return {'ty': 'Red'}
    elif color[0] == 'Blue':
        return {'ty': 'Blue'}
    elif color[0] == 'Green':
        return {'ty': 'Green'}
    elif color[0] == 'Yellow':
        return {'ty': 'Yellow'}

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
            return self.domain.reverse(**args)
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
    def reverse(self, str_):
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
    def reverse(self, str_):
        return '1' + str(str_)
    def age(self, id_):
        return {}
    def different(self, a, b):
        return {}
    def circleArea(self, circle):
        return {}
    def enemy(self, you):
        return {
            'Blue': ('Red',),
            'Red': ('Blue',),
            'Yellow': ('Green',),
            'Green': ('Yellow',)
        }[you[0]]
    def foo(self, either):
        return {}

# generated server runner

def run_server():
    app = Flask(__name__)
    api = Api(app)
    api.add_resource(Service, '/', resource_class_kwargs={'domain': DomainImpl()})
    app.run(debug=True)

if __name__ == '__main__':
    run_server()

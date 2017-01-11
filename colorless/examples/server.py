from flask import Flask, request, abort
from flask_restful import Resource, Api

# generated alias

class Years():
    def __init__(self, value):
        assert(type(value) == int)
        self.value = value

class Id_Person():
    def __init__(self, value):
        assert(type(value) == str)
        self.value = value

# generated sum type

class Either_int_str():
    def __init__(self, ctor, args):
        self.ctor = ctor
        self.args = args
    def Left(t0):
        assert(type(t0) == int)
        return Either_int_str('Left', [t0])
    def Right(t0):
        assert(type(t0) == str)
        return Either_int_str('Right', [t0])

class Color():
    def __init__(self, ctor):
        assert(type(ctor) == str)
        self.ctor = ctor
    def Red():
        return Color('Red')
    def Blue():
        return Color('Blue')
    def Yellow():
        return Color('Yellow')
    def Green():
        return Color('Green')

# generated product types

class Point_f64():
    def __init__(self, x, y):
        assert(type(x) == float)
        assert(type(y) == float)
        self.x = x
        self.y = y

class Circle():
    def __init__(self, center, radius):
        assert(type(center) == Point_f64)
        assert(type(radius) == float)
        self.center = center
        self.radius = radius

# generated json converter for alias

def json_to_years(json):
    return Years(json)

def years_to_json(years):
    assert(type(years) == Years)
    assert(type(years.value) == int)
    return years.value

def json_to_id_person(json):
    return Id_Person(json)

def id_person_to_json(id_):
    assert(type(id_) == Id_Person)
    assert(type(id_.value) == str)
    return id_.value

# generated json converter for sum type

def json_to_color(json):
    ctor = json['ctor']
    if ctor == 'Red':
        return Color.Red()
    elif ctor == 'Blue':
        return Color.Blue()
    elif ctor == 'Yellow':
        return Color.Yellow()
    elif ctor == 'Green':
        return Color.Green()

def color_to_json(color):
    ctor = color.ctor
    if ctor == 'Red':
        return {'ctor': 'Red'}
    elif ctor == 'Blue':
        return {'ctor': 'Blue'}
    elif ctor == 'Green':
        return {'ctor': 'Green'}
    elif ctor == 'Yellow':
        return {'ctor': 'Yellow'}

# generated json converter for product type

def json_to_point_f64(json):
    return Point_f64(
        x = json['x'],
        y = json['y'])

def point_f64_to_json(point_f64):
    return {
        'x': point_f64.x,
        'y': point_f64.y,
    }

def json_to_circle(json):
    return Circle(
        center = json_to_point_f64(json['center']),
        radius = json['radius'])

def circle_to_json(circle):
    return {
        'center': point_f64_to_json(circle.center),
        'radius': circle.radius,
    }

def json_to_either_int_str(json):
    ctor = json['ctor']
    if ctor == 'Left':
        return Either_int_str.Left(json['args'][0])
    elif ctor == 'Right':
        return Either_int_str.Right(json['args'][0])

# generated service

class Service(Resource):
    def __init__(self, domain):
        self.domain = domain

    def post(self):
        body = request.get_json()
        # meta = request.get_headers()

        fn = body.get('fn')
        args = body.get('args', {})

        try:
            if fn == 'reverse':
                return str(self.domain.reverse(a=str(args['a'])))
            elif fn == 'age':
                return years_to_json(self.domain.age(id_ = json_to_id_person(args['id'])))
            elif fn == 'different':
                return self.domain.different(a = json_to_id_person(args['a']), b = json_to_id_person(args['b']))
            elif fn == 'circleArea':
                return self.domain.circleArea(circle = json_to_circle(args['circle']))
            elif fn == 'enemy':
                return color_to_json(self.domain.enemy(you = json_to_color(args['you'])))
            elif fn == 'foo':
                return self.domain.foo(either = json_to_either_int_str(args['either']))
        except AssertionError:
            abort(500)

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
        return a.value != b.value
    def circleArea(self, circle):
        return circle.radius * circle.radius * 3.14
    def enemy(self, you):
        return {
            'Blue': Color.Red(),
            'Red': Color.Blue(),
            'Yellow': Color.Green(),
            'Green': Color.Yellow(),
        }[you.ctor]
    def foo(self, either):
        return True

# generated server runner

def run_server(domains):
    app = Flask(__name__)
    api = Api(app)
    api.add_resource(Service, '/', resource_class_kwargs={'domain': domains['Root']})
    app.run(debug=True)

if __name__ == '__main__':
    pass
    domains = { "Root": DomainImpl() }
    run_server(domains)

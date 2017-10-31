require "roda"
require "json"
require_relative "hello_world.rb"

class App < Roda
  plugin :json
  plugin :json_parser
  plugin :request_headers
  route do |r|
    r.get "" do
      response['Content-Type'] = 'application/json'
      get_spec
    end
    r.post "" do
      response['Content-Type'] = 'application/json'
      req = JSON.parse r.body.read
      handler(r.headers, req)
    end
  end
end

run App.freeze.app

require "roda"
require "./hello_world.rb"

class App < Roda
  plugin :json
  route do |r|
    r.root do
      response['Content-Type'] = 'application/json'
      get_spec
    end
  end
end

run App.freeze.app

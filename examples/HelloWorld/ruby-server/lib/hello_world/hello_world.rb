require_relative "v0.rb"
require 'json'

include V0

def get_spec
  V0.spec
end

def default_limits
  {
    :variables => 50,
    :service_calls => 50,
    :lambdas => 10,
    :expressions => 100,
  }
end

def default_hooks
  Proc.new { |xtra| {
      :meta_middleware => lambda { |meta| meta },
      :sandbox_limits => lambda { |meta2| default_limits },
    }
  }
end

def handler(headers, body)
  V0.handler(nil, default_hooks, headers, body)
end

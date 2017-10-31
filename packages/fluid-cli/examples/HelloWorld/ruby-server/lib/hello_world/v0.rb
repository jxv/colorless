require 'json'

def empty_env
  { :parent => nil, :vars => Hash.new }
end

class Eval
  def initialize(config, env)
    @config = config
    @env = env
  end
  def run(query)
    return query["n"]
  end
end

module V0
  def spec
    return "{\"fluid\":{\"major\":0,\"minor\":0},\"schema\":{\"Hello\":{\"m\":[{\"target\":\"String\"}],\"o\":\"String\"}},\"pull\":{\"protocol\":\"http\",\"name\":\"HelloWorld\",\"host\":\"127.0.0.1\",\"meta\":\"Unit\",\"path\":\"/\",\"port\":8080,\"error\":\"Unit\"},\"version\":{\"major\":0,\"minor\":0}}"
  end

  def self.handler(service, hooks_builder, xtra, req)
    begin
      hooks = hooks_builder.call xtra
      meta2 = hooks[:meta_middleware].call(req["meta"])
      env = empty_env
      limits = hooks[:sandbox_limits].call(meta2)
      limits[:variables] += env[:vars].size
      config = {
        :limits => limits,
        :lang_service_call_count => 0,
        :lang_lambda_count => 0,
        :lang_expr_count => 0,
        :meta2 => meta2,
        :service => service,
      }
      evaluator = Eval.new(config, env)
      { "tag" => "Success", "success" => evaluator.run(req["query"]) }
    rescue
      puts "some_error"
      return { "tag" => "Error", "error" => nil }
    end
  end
end

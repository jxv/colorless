require "json"

module V0
  def spec
    return "{\"fluid\":{\"major\":0,\"minor\":0},\"schema\":{\"Hello\":{\"m\":[{\"target\":\"String\"}],\"o\":\"String\"}},\"pull\":{\"protocol\":\"http\",\"name\":\"HelloWorld\",\"host\":\"127.0.0.1\",\"meta\":\"Unit\",\"path\":\"/\",\"port\":8080,\"error\":\"Unit\"},\"version\":{\"major\":0,\"minor\":0}}"
  end
end

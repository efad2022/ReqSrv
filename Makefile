#######
# Help
#######

.DEFAULT_GOAL := help
.PHONY: help

PWD=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

help:
	@echo ""
	@echo "*** ReqSrv Command line ***"
	@echo "NOTES:"
	@echo "- The development environment requires rebar3 and Erlang installed"
	@echo "- The cluster requires Docker and Docker Compose"
	@echo "- The development environment uses the test profile in rebar3 config"
	@echo "- The test profile mocks the request to the REST endpoint"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


.PHONY: deps
deps: ## get deps
	@./rebar3 as test deps

.PHONY: compile
compile: deps ## compile
	@./rebar3 as test deps

.PHONY: clean
clean: ## clean
	@./rebar3 as test clean 2>/dev/null
	@rm -rf _build 2>/dev/null

.PHONY: release
release: compile ## build release
	@./rebar3 as test release

.PHONY: shell
shell: compile ## start node locally, application boot with console
	@./rebar3 as test shell


.PHONY: cluster-up
cluster-up: compile ## cluster up
	@docker-compose -p "reqsrv" \
                    -f artifacts/cluster/docker-compose.yaml up \
                    --d

#.PHONY: cluster-shell-0
cluster-shell-0: ## shell in cluster dev-core.req-srv0
	@docker exec -it dev-core.req-srv0 /bin/bash

#.PHONY: cluster-shell-1
cluster-shell-1: ## shell in cluster dev-core.req-srv1
	@docker exec -it dev-core.req-srv1 /bin/bash

#.PHONY: cluster-shell-2
cluster-shell-2: ## shell in cluster dev-core.req-srv2
	@docker exec -it dev-core.req-srv2 /bin/bash

#.PHONY: cluster-attach-0
cluster-attach-0: ## attach to node in cluster dev-core.req-srv0
	@docker attach dev-core.req-srv0

#.PHONY: cluster-attach-1
cluster-attach-1: ## attach to node in cluster dev-core.req-srv1
	@docker attach dev-core.req-srv1

#.PHONY: cluster-attach-2
cluster-attach-2: ## attach to node in cluster dev-core.req-srv2
	@docker attach dev-core.req-srv2

#.PHONY: down
cluster-down: ## cluster down
	@docker-compose -p "reqsrv" -f artifacts/cluster/docker-compose.yaml stop
	@docker-compose -p "reqsrv" -f artifacts/cluster/docker-compose.yaml rm

.PHONY: test
test: ## send some sample requests
	./bin/stress.sh

DIRS = gamer wsgate master
clean:
	for dir in $(DIRS); do \
		rm -rf "./$$dir/dist"; \
	done

releaser:
	cd gamer && goreleaser --snapshot --skip-publish --rm-dist
	cd wsgate && goreleaser --snapshot --skip-publish --rm-dist
	cd master && goreleaser --snapshot --skip-publish --rm-dist

protobuf:
	cd gamer &&  protoc --go_out=. --proto_path=./proto ./proto/*.proto
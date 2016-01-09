REPO ?= actordb
PKG_REVISION ?= $(shell git describe --tags)
PKG_BUILD = 1
BASE_DIR = $(shell pwd)
ERLANG_BIN = $(shell dirname $(shell which erl))
REBAR ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

ifeq ($(uname_S),Darwin)
	SHELLCMD = gcc c_src/cmdshell.c -I/usr/local/Cellar/readline/6.3.8/include/ /usr/local/Cellar/readline/6.3.8/lib/libreadline.a -lncurses  -o priv/cmdshell
else
	SHELLCMD = gcc c_src/cmdshell.c -lreadline -lncurses  -o priv/cmdshell
endif

ifeq ($(uname_S),Darwin)
	TOOLCMD = gcc deps/actordb_driver/c_src/tool.c deps/actordb_driver/c_src/mdb.c deps/actordb_driver/c_src/midl.c deps/actordb_driver/c_src/lz4.c -D_TESTAPP_=1  -DMDB_MAXKEYSIZE=0 -DSQLITE_DEFAULT_PAGE_SIZE=4096 -DSQLITE_DEFAULT_WAL_AUTOCHECKPOINT=0  -o actordb_tool
else
	TOOLCMD = gcc deps/actordb_driver/c_src/tool.c deps/actordb_driver/c_src/mdb.c deps/actordb_driver/c_src/midl.c deps/actordb_driver/c_src/lz4.c -D_TESTAPP_=1  -DMDB_MAXKEYSIZE=0 -DSQLITE_DEFAULT_PAGE_SIZE=4096 -DSQLITE_DEFAULT_WAL_AUTOCHECKPOINT=0 -lpthread -ldl -o actordb_tool
endif

$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

all: deps compile

.PHONY: rel deps

# thrift -gen erl -out deps/adbt/src/  adbt.thrift
compile:
	$(SHELLCMD)
	$(TOOLCMD)
	./rebar compile
	./priv/mkconsole.escript



recompile:
	./rebar update-deps
	./rebar compile

update:
	./rebar update-deps

deps:
	./rebar get-deps

clean:
	./rebar clean


distclean: clean relclean ballclean
	./rebar delete-deps

generate:
	./rebar generate  $(OVERLAY_VARS)

rel: deps compile generate

relclean:
	rm -rf rel/riak

ballclean:
	rm -rf $(PKG_ID).tar.gz distdir

ARCH= $(shell uname -m)

##
## Version and naming variables for distribution and packaging
##

# Tag from git with style <tagname>-<commits_since_tag>-<current_commit_hash>
# Ex: When on a tag: actordb-1.0.3 (no commits since tag)
# For most normal Commits: actordb-1.1.0pre1-27-g1170096
# Last tag: actordb-1.1.0pre1
# Commits since tag: 27
# Hash of commit: g1170096
REPO_TAG         := $(shell git describe --tags)

# Split off repo name
# Changes to 1.0.3 or 1.1.0pre1-27-g1170096 from example above
REVISION = $(shell echo $(REPO_TAG) | sed -e 's/^$(REPO)-//')

# Primary version identifier, strip off commmit information
# Changes to 1.0.3 or 1.1.0pre1 from example above
MAJOR_VERSION        ?= $(shell echo $(REVISION) | sed -e 's/\([0-9.]*\)-.*/\1/')


##
## Release tarball creation
## Generates a tarball that includes all the deps sources so no checkouts are necessary
##

# Use git archive make a clean copy of a repository at a current
# revision and copy to a new directory
archive_git = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)

# Alternative to git archive to remove .git directory, but not any
# other files outside of the source tree (used for eleveldb which
# brings in leveldb)
clean_git = cp -R ../../$(1) $(2)/deps/ && find $(2)/$(1) -name .git -type d | xargs rm -rf

# Determines which function to call. eleveldb is treated as a special case
archive = $(call archive_git,$(1),$(2))


# Checkout tag, fetch deps (so we don't have to do it multiple times) and collect
# the version of all the dependencies into the MANIFEST_FILE
CLONEDIR ?= actordb-clone
MANIFEST_FILE ?= dependency_manifest.git
get_dist_deps = mkdir distdir && \
                git clone . distdir/$(CLONEDIR) && \
                cd distdir/$(CLONEDIR) && \
                git checkout $(REPO_TAG) && \
                $(MAKE) deps && \
                echo "- Dependencies and their tags at build time of $(REPO) at $(REPO_TAG)" > $(MANIFEST_FILE) && \
                for dep in deps/*; do \
                    cd $${dep} && \
                    printf "$${dep} version `git describe --long --tags 2>/dev/null || git rev-parse HEAD`\n" >> ../../$(MANIFEST_FILE) && \
                    cd ../..; done && \
                LC_ALL=POSIX && export LC_ALL && sort $(MANIFEST_FILE) > $(MANIFEST_FILE).tmp && mv $(MANIFEST_FILE).tmp $(MANIFEST_FILE);


# Name resulting directory & tar file based on current status of the git tag
# If it is a tagged release (PKG_VERSION == MAJOR_VERSION), use the toplevel
# tag as the package name, otherwise generate a unique hash of all the
# dependencies revisions to make the package name unique.
# This enables the toplevel repository package to change names
# when underlying dependencies change.
NAME_HASH = $(shell git hash-object distdir/$(CLONEDIR)/$(MANIFEST_FILE) 2>/dev/null | cut -c 1-8)
ifeq ($(REVISION), $(MAJOR_VERSION))
PKG_ID := $(REPO_TAG)
else
PKG_ID = $(REPO)-$(MAJOR_VERSION)-$(NAME_HASH)
endif

# To ensure a clean build, copy the CLONEDIR at a specific tag to a new directory
# which will be the basis of the src tar file (and packages)
# The vsn.git file is required by rebar to be able to build from the resulting
# tar file
build_clean_dir = cd distdir/$(CLONEDIR) && \
                  $(call archive_git,$(PKG_ID),..) && \
                  cp $(MANIFEST_FILE) ../$(PKG_ID)/ && \
                  mkdir ../$(PKG_ID)/deps && \
                  for dep in deps/*; do \
                      cd $${dep} && \
                           mkdir -p ../../../$(PKG_ID)/$${dep}/priv && \
                           $(call archive,$${dep},../../../$(PKG_ID)) && \
                           printf "`git describe --long --tags 2>/dev/null || git rev-parse HEAD`" > ../../../$(PKG_ID)/$${dep}/priv/vsn.git && \
                           cd ../..; \
                  done


distdir/$(CLONEDIR)/$(MANIFEST_FILE):
		$(if $(REPO_TAG), $(call get_dist_deps), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

distdir/$(PKG_ID): distdir/$(CLONEDIR)/$(MANIFEST_FILE)
		$(call build_clean_dir)

distdir/$(PKG_ID).tar.gz: distdir/$(PKG_ID)
		tar -C distdir -czf distdir/$(PKG_ID).tar.gz $(PKG_ID)

dist: distdir/$(PKG_ID).tar.gz
		cp distdir/$(PKG_ID).tar.gz .

ballclean:
		rm -rf $(PKG_ID).tar.gz distdir

pkgclean: ballclean
		rm -rf package

##
## Packaging targets
##

# Yes another variable, this one is repo-<generatedhash
# which differs from $REVISION that is repo-<commitcount>-<commitsha>
PKG_VERSION = $(shell echo $(PKG_ID) | sed -e 's/^$(REPO)-//')

package: distdir/$(PKG_ID).tar.gz
		ln -s distdir package
		$(MAKE) -C package -f $(PKG_ID)/deps/node_package/Makefile

.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE

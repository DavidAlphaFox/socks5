PROJECT = proxy
PROJECT_DESCRIPTION = Proxy over websocket
PROJECT_VERSION = 0.1.4
ERLC_OPTS = -W +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

DEPS = ranch ailib aiconf aiutp

dep_ranch_commit = 1.7.1
dep_ailib = git https://github.com/DavidAlphaFox/ailib.git v0.4.4
dep_aiconf = git https://github.com/DavidAlphaFox/aiconf.git v0.1.7
dep_aiutp = git ssh://git@github.com/DavidAlphaFox/aiutp.git

include erlang.mk

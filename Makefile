APP = hercule 
ORG = fogfish
URI = 

include erlang.mk

##
## 
react:
	mkdir -p /tmp/hercule-console/node_modules ;\
	cd apps/hercule/priv/hercule-console ;\
	ln -s /tmp/hercule-console/node_modules node_modules;\
	npm install ;\
	PUBLIC_URL=/hercule npm run build ;\
	rm node_modules ;\
	cd -
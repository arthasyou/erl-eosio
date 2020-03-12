%%%--------------------------------------------------
%%%  自动生成,请勿编辑
%%%--------------------------------------------------
-module(data_eos_url).
-include("logger.hrl").
-compile(export_all).
get(1) -> {data_eos_url,1,"http://peer1.eoshuobipool.com:8181","火币1","use_false",""};
get(2) -> {data_eos_url,2,"http://mainnet.meet.one","meet","use_false",""};
get(3) -> {data_eos_url,3,"http://api-mainnet.starteos.io","mainnet_starteos","use_false",""};
get(4) -> {data_eos_url,4,"https://api.eoslaomao.com","japan","use_false",""};
get(5) -> {data_eos_url,5,"https://api.bitmars.one","Tokyo","use_false",""};
get(6) -> {data_eos_url,6,"https://publicapi-mainnet.eosauthority.com","London","use_false",""};
get(7) -> {data_eos_url,7,"http://eosbp-0.atticlab.net","Kyiv","use_false",""};
get(8) -> {data_eos_url,8,"https://eos.newdex.one","Hong_Kong","use_false",""};
get(9) -> {data_eos_url,9,"http://api.eosbeijing.one","Tokyo","use_false",""};
get(Id) -> throw({badarg,Id,data_eos_url}).
list() -> [1,2,3,4,5,6,7,8,9].

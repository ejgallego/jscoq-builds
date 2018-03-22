function(ad){"use strict";var
M=250,L="Ascii",p=246,K="ascii",b=ad.jsoo_runtime,I=b.caml_ml_string_length,c=b.caml_new_string,H=b.caml_obj_tag,J=b.caml_register_global,n=b.caml_string_get,ab=b.caml_wrap_exception;function
d(a,c){return a.length==1?a(c):b.caml_call_gen(a,[c])}function
e(a,c,d){return a.length==2?a(c,d):b.caml_call_gen(a,[c,d])}function
G(a,c,d,e){return a.length==3?a(c,d,e):b.caml_call_gen(a,[c,d,e])}function
ac(a,c,d,e,f){return a.length==4?a(c,d,e,f):b.caml_call_gen(a,[c,d,e,f])}var
a=b.caml_get_global_data(),q=c("ascii_syntax_plugin"),i=[0,c("Coq"),[0,c("Strings"),[0,c(L),0]]],h=a.Util,j=a.Coqlib,g=a.DAst,A=a.CamlinternalLazy,s=a.Globnames,l=a.Names,Z=a.Option,Y=a.Pervasives,X=a.Printf,T=a.Pp,V=a.CErrors,N=a.Libnames,aa=a.Notation;d(a.Mltop[10],q);var
k=[248,c("Ascii_syntax.Non_closed_ascii"),b.caml_fresh_oo_id(0)],W=[0,[4,0,[0,2,3],0,0],c("%03d")],S=c("Expects a single character or a three-digits ascii code."),U=[0,c("interp_ascii_string")],R=c(L),Q=c("Ascii interpretation"),O=c(K),P=c(K),$=c("char_scope");function
o(a){var
b=e(h[17][17],l[1][6],a);return d(l[5][4],b)}function
r(b,a){var
c=d(l[1][6],a),f=o(b);return e(s[25],f,c)}function
t(b,a){var
c=d(l[1][6],a),f=o(b);return e(N[17],f,c)}function
m(c,b){var
a=d(g[1],c);return 0===a[0]?e(s[5],a[1],b):0}var
u=t(i,O),v=r(i,P),w=[0,[0,v,0],1],x=[3,w];function
y(a){return G(j[1],Q,i,a)}var
f=[p,function(a){return y(R)}];function
z(a,h){function
b(d,c){if(0===d)return 0;var
f=b(d-1|0,c/2|0),h=0,i=0===(c%2|0)?j[27]:j[26];return[0,e(g[3],a,[0,i,h]),f]}var
c=H(f),i=b(8,h),k=0,l=M===c?f[1]:p===c?d(A[2],f):f,m=[4,e(g[3],a,[0,l,k]),i];return e(g[3],a,m)}function
B(f,a){if(1===I(a))var
e=n(a,0);else{if(3===I(a)){var
g=n(a,0);if(d(h[11],g)){var
i=n(a,1);if(d(h[11],i)){var
j=n(a,2);if(d(h[11],j))var
e=b.caml_int_of_string(a),c=1;else
var
c=0}else
var
c=0}else
var
c=0}else
var
c=0;if(!c)var
k=d(T[3],S),e=G(V[6],f,U,k)}return z(f,e)}function
C(b){function
a(c,b){if(b){var
d=b[2],e=b[1];if(m(e,j[26]))return 1+(2*a(c-1|0,d)|0)|0;if(m(e,j[27]))return 2*a(c-1|0,d)|0}else
if(0===c)return 0;throw k}try{var
c=[0,function(e){var
b=d(g[1],e);if(4===b[0]){var
c=H(f),h=b[2],i=b[1],j=M===c?f[1]:p===c?d(A[2],f):f;if(m(i,j))return a(8,h)}throw k}(b)];return c}catch(a){a=ab(a);if(a===k)return 0;throw a}}function
D(a){if(32<=a)if(!(126<a)){var
b=d(Y[17],a);return e(h[15][1],1,b)}return e(X[4],W,a)}function
E(a){var
b=C(a[1]);return e(Z[16],D,b)}var
_=[0,[0,e(g[3],0,[0,x,0]),0],E,1];ac(aa[15],$,[0,u,i],B,_);var
F=[0,q,k,o,r,t,m,i,u,v,w,x,y,f,z,B,C,D,E];J(25,F,"Ascii_syntax_plugin.Ascii_syntax");J(26,[0,F],"Ascii_syntax_plugin");return}

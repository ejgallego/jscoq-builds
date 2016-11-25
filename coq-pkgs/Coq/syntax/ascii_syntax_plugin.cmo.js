(function(ab){"use strict";var
J=250,I="Ascii",o=246,H="ascii",b=ab.jsoo_runtime,F=b.caml_ml_string_length,c=b.caml_new_string,E=b.caml_obj_tag,G=b.caml_register_global,m=b.caml_string_get,$=b.caml_wrap_exception;function
d(a,c){return a.length==1?a(c):b.caml_call_gen(a,[c])}function
f(a,c,d){return a.length==2?a(c,d):b.caml_call_gen(a,[c,d])}function
_(a,c,d,e){return a.length==3?a(c,d,e):b.caml_call_gen(a,[c,d,e])}function
aa(a,c,d,e,f){return a.length==4?a(c,d,e,f):b.caml_call_gen(a,[c,d,e,f])}var
a=b.caml_get_global_data(),q=c("ascii_syntax_plugin"),i=[0,c("Coq"),[0,c("Strings"),[0,c(I),0]]],g=a.Util,h=a.Coqlib,k=a.Globnames,p=a.CamlinternalLazy,j=a.Names,N=a.Option,M=a.Pervasives,K=a.Printf,L=a.Pp,R=a.CErrors,Q=a.Libnames,O=a.Notation,P=a.Loc;d(a.Mltop[12],q);var
l=[248,c("Ascii_syntax.Non_closed_ascii"),b.caml_fresh_oo_id(0)],Y=[0,[4,0,[0,2,3],0,0],c("%03d")],W=c("Expects a single character or a three-digits ascii code."),X=c("interp_ascii_string"),V=c(I),U=c("Ascii interpretation"),S=c(H),T=c(H),Z=c("char_scope");function
n(a){var
b=f(g[17][14],j[1][5],a);return d(j[5][4],b)}function
r(b,a){var
c=d(j[1][5],a),e=n(b);return f(k[25],e,c)}function
s(b,a){var
c=d(j[1][5],a),e=n(b);return f(Q[17],e,c)}var
t=s(i,S),u=r(i,T),v=[0,[0,u,0],1],w=[3,v];function
x(a){return _(h[1],U,i,a)}var
e=[o,function(a){return x(V)}];function
y(a,f){function
b(d,c){if(0===d)return 0;var
e=b(d-1|0,c/2|0),f=0,g=0===(c%2|0)?h[29]:h[28];return[0,[0,[0,a,g,f]],e]}var
c=E(e),g=b(8,f),i=0,j=J===c?e[1]:o===c?d(p[2],e):e;return[4,a,[0,[0,a,j,i]],g]}function
z(f,a){if(1===F(a))var
e=m(a,0);else{if(3===F(a)){var
h=m(a,0);if(d(g[11],h)){var
i=m(a,1);if(d(g[11],i)){var
j=m(a,2);if(d(g[11],j))var
e=b.caml_int_of_string(a),c=1;else
var
c=0}else
var
c=0}else
var
c=0}else
var
c=0;if(!c)var
k=[0,f,X,d(L[1],W)],e=d(R[8],k)}return y(f,e)}function
A(b){function
a(c,b){if(b){var
d=b[1];if(0===d[0]){var
e=b[2],g=d[1][2];if(f(k[5],g,h[28]))return 1+(2*a(c-1|0,e)|0)|0;if(f(k[5],g,h[29]))return 2*a(c-1|0,e)|0}}else
if(0===c)return 0;throw l}try{var
c=[0,function(b){if(4===b[0]){var
c=b[2];if(0===c[0]){var
g=E(e),h=b[3],i=c[1][2],j=J===g?e[1]:o===g?d(p[2],e):e;if(f(k[5],i,j))return a(8,h)}}throw l}(b)];return c}catch(a){a=$(a);if(a===l)return 0;throw a}}function
B(a){if(32<=a)if(!(126<a)){var
b=d(M[17],a);return f(g[15][1],1,b)}return f(K[4],Y,a)}function
C(a){var
b=A(a);return f(N[15],B,b)}aa(O[14],Z,[0,t,i],z,[0,[0,[0,[0,P[4],w,0]],0],C,1]);var
D=[0,q,l,n,r,s,i,t,u,v,w,x,e,y,z,A,B,C];G(25,D,"Ascii_syntax_plugin.Ascii_syntax");G(26,[0,D],"Ascii_syntax_plugin");return});

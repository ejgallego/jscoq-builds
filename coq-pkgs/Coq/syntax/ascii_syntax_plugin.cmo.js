(function(ab){"use strict";var
K=250,J="Ascii",o=246,I="ascii",b=ab.jsoo_runtime,G=b.caml_ml_string_length,c=b.caml_new_string,F=b.caml_obj_tag,H=b.caml_register_global,m=b.caml_string_get,$=b.caml_wrap_exception;function
d(a,c){return a.length==1?a(c):b.caml_call_gen(a,[c])}function
f(a,c,d){return a.length==2?a(c,d):b.caml_call_gen(a,[c,d])}function
E(a,c,d,e){return a.length==3?a(c,d,e):b.caml_call_gen(a,[c,d,e])}function
aa(a,c,d,e,f){return a.length==4?a(c,d,e,f):b.caml_call_gen(a,[c,d,e,f])}var
a=b.caml_get_global_data(),p=c("ascii_syntax_plugin"),h=[0,c("Coq"),[0,c("Strings"),[0,c(J),0]]],g=a.Util,i=a.Coqlib,l=a.Globnames,y=a.CamlinternalLazy,k=a.Names,X=a.Option,W=a.Pervasives,V=a.Printf,R=a.Pp,T=a.CErrors,L=a.Libnames,Y=a.Loc,_=a.Notation;d(a.Mltop[12],p);var
j=[248,c("Ascii_syntax.Non_closed_ascii"),b.caml_fresh_oo_id(0)],U=[0,[4,0,[0,2,3],0,0],c("%03d")],Q=c("Expects a single character or a three-digits ascii code."),S=[0,c("interp_ascii_string")],P=c(J),O=c("Ascii interpretation"),M=c(I),N=c(I),Z=c("char_scope");function
n(a){var
b=f(g[17][14],k[1][6],a);return d(k[5][4],b)}function
q(b,a){var
c=d(k[1][6],a),e=n(b);return f(l[25],e,c)}function
r(b,a){var
c=d(k[1][6],a),e=n(b);return f(L[17],e,c)}var
s=r(h,M),t=q(h,N),u=[0,[0,t,0],1],v=[3,u];function
w(a){return E(i[1],O,h,a)}var
e=[o,function(a){return w(P)}];function
x(a,f){function
b(d,c){if(0===d)return 0;var
e=b(d-1|0,c/2|0),f=0,g=0===(c%2|0)?i[29]:i[28];return[0,[0,[0,a,g,f]],e]}var
c=F(e),g=b(8,f),h=0,j=K===c?e[1]:o===c?d(y[2],e):e;return[4,a,[0,[0,a,j,h]],g]}function
z(f,a){if(1===G(a))var
e=m(a,0);else{if(3===G(a)){var
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
k=d(R[3],Q),e=E(T[6],[0,f],S,k)}return x(f,e)}function
A(b){function
a(c,b){if(b){var
d=b[1];if(0===d[0]){var
e=b[2],g=d[1][2];if(f(l[5],g,i[28]))return 1+(2*a(c-1|0,e)|0)|0;if(f(l[5],g,i[29]))return 2*a(c-1|0,e)|0}}else
if(0===c)return 0;throw j}try{var
c=[0,function(b){if(4===b[0]){var
c=b[2];if(0===c[0]){var
g=F(e),h=b[3],i=c[1][2],k=K===g?e[1]:o===g?d(y[2],e):e;if(f(l[5],i,k))return a(8,h)}}throw j}(b)];return c}catch(a){a=$(a);if(a===j)return 0;throw a}}function
B(a){if(32<=a)if(!(126<a)){var
b=d(W[17],a);return f(g[15][1],1,b)}return f(V[4],U,a)}function
C(a){var
b=A(a);return f(X[15],B,b)}aa(_[14],Z,[0,s,h],z,[0,[0,[0,[0,Y[4],v,0]],0],C,1]);var
D=[0,p,j,n,q,r,h,s,t,u,v,w,e,x,z,A,B,C];H(25,D,"Ascii_syntax_plugin.Ascii_syntax");H(26,[0,D],"Ascii_syntax_plugin");return});

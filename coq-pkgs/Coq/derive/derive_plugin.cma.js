function(R){"use strict";var
s="Derive",r=142,c=R.jsoo_runtime,d=c.caml_new_string,j=c.caml_register_global;function
b(a,b){return a.length==1?a(b):c.caml_call_gen(a,[b])}function
q(a,b,d){return a.length==2?a(b,d):c.caml_call_gen(a,[b,d])}function
Q(a,b,d,e){return a.length==3?a(b,d,e):c.caml_call_gen(a,[b,d,e])}function
k(a,b,d,e,f){return a.length==4?a(b,d,e,f):c.caml_call_gen(a,[b,d,e,f])}function
l(a,b,d,e,f,g){return a.length==5?a(b,d,e,f,g):c.caml_call_gen(a,[b,d,e,f,g])}var
a=c.caml_get_global_data(),o=d("derive_plugin"),n=a.Proofview,f=a.EConstr,e=a.Evd,g=a.Lemmas,h=a.Stdarg,i=a.Genarg,C=a.Proof,D=a.Util,v=a.Context,w=a.Environ,y=a.Constrintern,t=a.Global,E=a.Proof_global,H=a.Attributes,z=a.Mltop,P=a.Vernacextend;j(8,[0,0,0],"Derive_plugin");var
x=[0,0],u=[1,0],A=[0,[0,1,0]],I=d("As"),K=d("SuchThat"),M=d(s),O=d(s);function
m(d,i,c){var
a=b(t[2],0),j=b(e[17],a),h=k(e[135],0,0,e[129],j),o=h[1],m=0,p=b(f[14],h[2]),s=[1,a,o,p,function(e,c){return[1,a,e,c,function(j,h){var
k=b(f[r][1],c),m=b(f[r][1],h),n=[1,b(v[7],d),m,k],e=q(w[41],n,a),g=l(y[16],x,e,j,0,i),o=g[2],p=g[1];return[1,e,p,o,function(a,b){return[0,a]}]}]}],z=[0,l(g[7][1],0,[0,[1,d,c]],0,[0,u],0)],A=l(g[9],c,m,0,z,s);function
B(c){var
d=k(n[31],0,1,2,n[41]),e=Q(C[24],a,d,c);return b(D[12],e)}var
F=b(E[14],B);return q(g[2],F,A)}j(20,[0,m],"Derive_plugin__Derive");b(z[9],o);function
p(a){return A}var
B=0,F=0;function
G(e,d,c,a){b(H[2],a);return[3,function(a){return m(e,d,c)}]}var
J=[0,I,[1,[5,b(i[16],h[7])],0]],L=[0,K,[1,[5,b(i[16],h[11])],J]],N=[0,[0,0,[0,M,[1,[5,b(i[16],h[7])],L]],G,F],B];k(P[2],O,[0,p],0,N);j(26,[0,o,p],"Derive_plugin__G_derive");return}

function(bb){"use strict";var
C="f",x="X2",Q="Logic",g="X1",l="tauto_flags",w="id",R="Coq",k=bb.jsoo_runtime,a=k.caml_new_string,P=k.caml_register_global,a$=k.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):k.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):k.caml_call_gen(a,[b,c])}function
B(a,b,c,d){return a.length==3?a(b,c,d):k.caml_call_gen(a,[b,c,d])}function
r(a,b,c,d,e){return a.length==4?a(b,c,d,e):k.caml_call_gen(a,[b,c,d,e])}function
ba(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):k.caml_call_gen(a,[b,c,d,e,f])}var
d=k.caml_get_global_data(),y=a("tauto_plugin"),f=d.Names,s=d.Ltac_plugin,q=d.Util,v=d.Loc,D=d.Mltop,N=d.CAst,j=d.Tacticals,e=d.Proofview,G=d.Pp,o=d.EConstr,m=d.Tactics,n=d.Hipattern,E=d.Geninterp,aO=d.Nametab,aN=d.Not_found,aD=d.Locusops,aj=d.Global,X=d.Assert_failure,T=d.Stdlib,aa=d.Goptions,aK=d.Libnames;c(D[10],y);var
aR=a(C),aS=a("x"),aE=[22,0],aw=[0,a(Q),[0,a("Init"),[0,a(R),0]]],at=a(g),au=a(x),av=a(w),ar=a(g),an=a(g),ao=a(x),ap=a(w),al=a(g),ai=a(g),ag=a(g),V=a(l),W=[0,a("plugins/ltac/tauto.ml"),61,12],S=a("tauto: anomaly"),U=a(l),Z=[0,a("Intuition"),[0,a("Negation"),[0,a("Unfolding"),0]]],_=a("unfolding of not in intuition"),ad=[0,0,0],aB=a("not"),aF=[0,a("Classical_Prop"),[0,a(Q),[0,a(R),0]]],aH=a("NNPP"),aP=[0,1,0,1,1,0],aQ=[0,0,0,0,0,0],aT=[0,a(l),[0,a(g),0]],aU=a("is_empty"),aV=[0,a(l),[0,a(g),0]],aW=a("is_unit_or_eq"),aX=[0,a(l),[0,a(g),0]],aY=a("is_disj"),aZ=[0,a(l),[0,a(g),0]],a0=a("is_conj"),a1=[0,a(l),[0,a(g),[0,a(x),[0,a(w),0]]]],a2=a("flatten_contravariant_disj"),a3=[0,a(l),[0,a(g),[0,a(x),[0,a(w),0]]]],a4=a("flatten_contravariant_conj"),a5=a("apply_nnpp"),a6=a("reduction_not_iff"),a7=[0,a(C),0],a8=a("with_uniform_flags"),a9=[0,a(C),0],a_=a("with_power_flags");function
h(e,d){var
g=d[1],h=c(f[1][6],e),i=b(f[1][11][22],h,g),a=c(s[13][2][2],i);return a?a[1]:c(T[3],S)}var
F=c(E[1][1],U);function
t(d){var
e=d[1],g=c(f[1][6],V),a=b(f[1][11][22],g,e),h=a[2];if(b(E[1][2],a[1],F))return h;throw[0,X,W]}var
z=[0,1];function
Y(a){z[1]=a;return 0}var
$=[0,0,_,Z,function(a){return z[1]},Y];b(aa[4],0,$);var
u=c(e[16],0),ab=c(G[7],0),ac=b(j[66][4],0,ab),p=c(e[40],ac),H=m[16];function
I(a,b){var
d=a?[0,[0,a[1]]]:0,f=r(m[142],1,d,0,b);return c(e[40],f)}function
A(a){return c(m[86],a)}function
J(a){return c(m[75],[0,a,0])}var
K=m[41],ae=b(m[116],0,ad);function
af(d,a){function
c(c){var
d=h(ag,a);return b(n[12],c,d)?u:p}return b(e[72][1],e[55],c)}function
ah(d,a){function
c(c){var
d=t(a)[5]?n[15]:n[14];return b(d,c,h(ai,a))?u:p}return b(e[72][1],e[55],c)}function
L(a,d){var
e=b(o[51],a,d);if(e){var
g=b(o[83],a,d)[1],f=b(o[3],a,g);return 11===f[0]?2===c(aj[28],f[1][1])[1][6]?1:0:0}return e}function
ak(d,c){function
a(b){var
a=t(c),d=h(al,c),e=a[2]?L(b,d)?0:1:0;if(!e)if(r(n[6],[0,a[4]],[0,a[1]],b,d))return u;return p}return b(e[72][1],e[55],a)}function
am(f,a){function
d(d){var
e=t(a),k=h(an,a),l=h(ao,a),f=h(ap,a),g=r(n[5],[0,e[3]],[0,e[1]],d,k);if(g){var
i=g[1][2],m=B(q[17][16],o[33],i,l),s=function(a){return H},u=b(j[66][23],s,i),v=[0,u,[0,A(f),[0,ae,[0,K,0]]]],w=c(j[66][22],v),x=[0,J(b(o[68],d,f)),0],y=[0,I([0,w],m),x];return c(j[66][22],y)}return p}return b(e[72][1],e[55],d)}function
aq(d,c){function
a(b){var
a=t(c),d=h(ar,c),e=a[2]?L(b,d)?0:1:0;if(!e)if(r(n[4],[0,a[4]],[0,a[1]],b,d))return u;return p}return b(e[72][1],e[55],a)}function
as(f,a){function
d(d){var
e=t(a),i=h(at,a),k=h(au,a),f=h(av,a),g=r(n[3],[0,e[3]],[0,e[1]],d,i);if(g){var
l=g[1][2],s=function(d,a){var
e=b(o[33],a,k),g=[0,r(m[108],0,0,d+1|0,0),[0,K,0]],h=[0,H,[0,A(f),g]];return I([0,c(j[66][22],h)],e)},u=b(q[17][13],s,l),v=J(b(o[68],d,f)),w=c(j[66][22],u);return b(j[66][3],w,v)}return p}return b(e[72][1],e[55],d)}var
ax=b(q[17][69],f[1][6],aw),ay=c(f[5][4],ax),az=c(f[6][4],aB),aA=[0,0,[0,[0,[1,b(f[17][3],[0,ay],az)],0]]];function
aC(d,a){var
c=0===z[1]?aE:[0,b(v[11],0,[10,[5,[0,aA,0]],aD[4]])];return b(s[13][23],a,c)}var
aG=b(q[17][69],f[1][6],aF),aI=c(f[1][6],aH),aJ=c(f[5][4],aG),aL=b(aK[15],aJ,aI);function
aM(g,f){function
a(h){try{var
a=c(aO[24],aL),f=c(j[66][61],a),g=b(e[72][1],f,A);return g}catch(a){a=a$(a);if(a===aN){var
d=c(G[7],0);return b(j[66][4],0,d)}throw a}}var
d=c(e[16],0);return b(e[17],d,a)}function
M(e,n,a){var
g=c(f[1][6],aR),h=b(N[1],0,g),i=c(f[1][6],aS),d=b(N[1],0,i),j=a[2],k=[0,B(f[1][11][4],d[1],[0,F,e],a[1]),j],l=[3,b(v[11],0,[0,[1,h],[0,[2,[1,d]],0]])],m=[29,b(v[11],0,l)];return b(s[13][23],k,m)}function
i(g,a,e){function
h(a){return c(f[1][6],a)}var
i=b(q[17][69],h,e);function
j(a){return[0,a]}var
d=[0,y,a],k=b(q[17][69],j,i);B(s[4][16],0,d,[0,g]);var
l=[28,[0,k,[31,b(v[11],0,[0,[0,d,0],0])]]];function
m(d){var
b=c(f[1][6],a);return ba(s[4][10],1,1,0,b,l)}return b(D[14],m,y)}i(af,aU,aT);i(ah,aW,aV);i(aq,aY,aX);i(ak,a0,aZ);i(as,a2,a1);i(am,a4,a3);i(aM,a5,0);i(aC,a6,0);i(function(a,b){return M(aP,a,b)},a8,a7);i(function(a,b){return M(aQ,a,b)},a_,a9);var
O=[0];P(67,O,"Tauto_plugin.Tauto");P(68,[0,O],"Tauto_plugin");return}

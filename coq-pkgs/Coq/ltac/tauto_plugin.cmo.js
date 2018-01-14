(function(bg){"use strict";var
X="Unfolding",D="f",x="X2",V="Logic",g="X1",l="tauto_flags",U="Intuition",w="id",W="Coq",k=bg.jsoo_runtime,a=k.caml_new_string,T=k.caml_register_global,bf=k.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):k.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):k.caml_call_gen(a,[b,c])}function
C(a,b,c,d){return a.length==3?a(b,c,d):k.caml_call_gen(a,[b,c,d])}function
p(a,b,c,d,e){return a.length==4?a(b,c,d,e):k.caml_call_gen(a,[b,c,d,e])}var
d=k.caml_get_global_data(),y=a("tauto_plugin"),f=d.Names,t=d.Ltac_plugin,r=d.Util,s=d.Loc,E=d.Mltop,j=d.Tacticals,e=d.Proofview,I=d.Pp,o=d.EConstr,m=d.Tactics,n=d.Hipattern,F=d.Geninterp,H=d.Goptions,aU=d.Nametab,aT=d.Not_found,aJ=d.Locusops,as=d.Global,ab=d.Assert_failure,Z=d.Pervasives,aQ=d.Libnames;c(E[12],y);var
aX=a(D),aY=a("x"),aK=[22,0],aF=[0,a(V),[0,a("Init"),[0,a(W),0]]],aC=a(g),aD=a(x),aE=a(w),aA=a(g),aw=a(g),ax=a(x),ay=a(w),au=a(g),ar=a(g),ap=a(g),$=a(l),aa=[0,a("plugins/ltac/tauto.ml"),60,12],Y=a("tauto: anomaly"),_=a(l),ad=[0,a(U),[0,a("Negation"),[0,a(X),0]]],ae=a("unfolding of not in intuition"),ah=[0,a(U),[0,a("Iff"),[0,a(X),0]]],ai=a("unfolding of iff in intuition"),am=[0,0,0],aG=a("iff"),aH=a("not"),aL=[0,a("Classical_Prop"),[0,a(V),[0,a(W),0]]],aN=a("NNPP"),aV=[0,1,0,1,1,0],aW=[0,0,0,0,0,0],aZ=[0,a(l),[0,a(g),0]],a0=a("is_empty"),a1=[0,a(l),[0,a(g),0]],a2=a("is_unit_or_eq"),a3=[0,a(l),[0,a(g),0]],a4=a("is_disj"),a5=[0,a(l),[0,a(g),0]],a6=a("is_conj"),a7=[0,a(l),[0,a(g),[0,a(x),[0,a(w),0]]]],a8=a("flatten_contravariant_disj"),a9=[0,a(l),[0,a(g),[0,a(x),[0,a(w),0]]]],a_=a("flatten_contravariant_conj"),a$=a("apply_nnpp"),ba=a("reduction_not_iff"),bb=[0,a(D),0],bc=a("with_uniform_flags"),bd=[0,a(D),0],be=a("with_power_flags");function
h(e,d){var
g=d[1],h=c(f[1][6],e),i=b(f[1][11][22],h,g),a=c(t[12][2][2],i);return a?a[1]:c(Z[2],Y)}var
G=c(F[1][1],_);function
u(d){var
e=d[1],g=c(f[1][6],$),a=b(f[1][11][22],g,e),h=a[2];if(b(F[1][2],a[1],G))return h;throw[0,ab,aa]}var
z=[0,1],A=[0,0];function
ac(a){z[1]=a;return 0}var
af=[0,0,ae,ad,function(a){return z[1]},ac];b(H[4],0,af);function
ag(a){A[1]=a;return 0}var
aj=[0,1,ai,ah,function(a){return A[1]},ag];b(H[4],0,aj);var
v=c(e[13],0),ak=c(I[7],0),al=b(j[66][4],0,ak),q=c(e[37],al),J=m[16];function
K(a,b){var
d=a?[0,[0,a[1]]]:0,f=p(m[143],1,d,0,b);return c(e[37],f)}function
B(a){return c(m[85],a)}function
L(a){return c(m[74],[0,a,0])}var
M=m[41],an=b(m[117],0,am);function
ao(d,a){function
c(c){var
d=h(ap,a);return b(n[12],c,d)?v:q}return b(e[68][1],e[51],c)}function
aq(d,a){function
c(c){var
d=u(a)[5]?n[15]:n[14];return b(d,c,h(ar,a))?v:q}return b(e[68][1],e[51],c)}function
N(a,d){var
e=b(o[51],a,d);if(e){var
g=b(o[81],a,d)[1],f=b(o[3],a,g);return 11===f[0]?2===c(as[26],f[1][1])[1][6]?1:0:0}return e}function
at(d,c){function
a(b){var
a=u(c),d=h(au,c),e=a[2]?N(b,d)?0:1:0;if(!e)if(p(n[6],[0,a[4]],[0,a[1]],b,d))return v;return q}return b(e[68][1],e[51],a)}function
av(f,a){function
d(d){var
e=u(a),k=h(aw,a),l=h(ax,a),f=h(ay,a),g=p(n[5],[0,e[3]],[0,e[1]],d,k);if(g){var
i=g[1][2],m=C(r[17][19],o[33],i,l),s=function(a){return J},t=b(j[66][21],s,i),v=[0,t,[0,B(f),[0,an,[0,M,0]]]],w=c(j[66][20],v),x=[0,L(b(o[66],d,f)),0],y=[0,K([0,w],m),x];return c(j[66][20],y)}return q}return b(e[68][1],e[51],d)}function
az(d,c){function
a(b){var
a=u(c),d=h(aA,c),e=a[2]?N(b,d)?0:1:0;if(!e)if(p(n[4],[0,a[4]],[0,a[1]],b,d))return v;return q}return b(e[68][1],e[51],a)}function
aB(f,a){function
d(d){var
e=u(a),i=h(aC,a),k=h(aD,a),f=h(aE,a),g=p(n[3],[0,e[3]],[0,e[1]],d,i);if(g){var
l=g[1][2],s=function(d,a){var
e=b(o[33],a,k),g=[0,p(m[109],0,0,d+1|0,0),[0,M,0]],h=[0,J,[0,B(f),g]];return K([0,c(j[66][20],h)],e)},t=b(r[17][16],s,l),v=L(b(o[66],d,f)),w=c(j[66][20],t);return b(j[66][3],w,v)}return q}return b(e[68][1],e[51],d)}function
O(a){var
d=b(r[17][15],f[1][6],aF),e=c(f[5][4],d),g=c(f[6][4],a);return[0,0,[0,[0,[1,b(f[17][3],[0,e],g)],0]]]}var
P=O(aG),Q=O(aH);function
aI(f,d){function
a(a){return[0,b(s[10],0,[10,[5,a],aJ[4]])]}var
c=A[1],e=0===z[1]?0===c?aK:a([0,P,0]):0===c?a([0,Q,0]):a([0,Q,[0,P,0]]);return b(t[12][22],d,e)}var
aM=b(r[17][15],f[1][6],aL),aO=c(f[1][6],aN),aP=c(f[5][4],aM),aR=b(aQ[17],aP,aO);function
aS(g,f){function
a(h){try{var
a=c(aU[24],aR),f=c(j[66][58],a),g=b(e[68][1],f,B);return g}catch(a){a=bf(a);if(a===aT){var
d=c(I[7],0);return b(j[66][4],0,d)}throw a}}var
d=c(e[13],0);return b(e[14],d,a)}function
R(e,n,a){var
g=c(f[1][6],aX),h=b(s[10],0,g),i=c(f[1][6],aY),d=b(s[10],0,i),j=a[2],k=[0,C(f[1][11][4],d[2],[0,G,e],a[1]),j],l=[3,b(s[10],0,[0,[1,h],[0,[2,[1,d]],0]])],m=[29,b(s[10],0,l)];return b(t[12][22],k,m)}function
i(g,a,e){function
h(a){return c(f[1][6],a)}var
i=b(r[17][15],h,e);function
j(a){return[0,a]}var
d=[0,y,a],k=b(r[17][15],j,i);C(t[6][9],0,d,[0,g]);var
l=[28,[0,k,[31,b(s[10],0,[0,[0,d,0],0])]]];function
m(d){var
b=c(f[1][6],a);return p(t[6][4],1,1,b,l)}return b(E[19],m,y)}i(ao,a0,aZ);i(aq,a2,a1);i(az,a4,a3);i(at,a6,a5);i(aB,a8,a7);i(av,a_,a9);i(aS,a$,0);i(aI,ba,0);i(function(a,b){return R(aV,a,b)},bc,bb);i(function(a,b){return R(aW,a,b)},be,bd);var
S=[0];T(69,S,"Tauto_plugin.Tauto");T(70,[0,S],"Tauto_plugin");return});

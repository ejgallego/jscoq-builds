function(gz){"use strict";var
bV="Only identifiers are allowed here",b4="partial term ",U="in",bU="mk_tpattern_matcher with no upats_origin.",ac=246,b3="(",b2=152,a3=160,ab="In",bX="ssrpattern",b1="pattern",ar="_vendor+v8.11+32bit/coq/plugins/ssrmatching/ssrmatching.ml",a7=159,a1="As",u=" in ",bT="_ssrpat_",a4=120,b0="The ",r=142,aD=102,aa="in ",V=175,bZ="do_once never called.",a6="ssrmatching_plugin",bS="_",bY="Qed",bW=" of ",a5=248,a2=" as ",o=gz.jsoo_runtime,I=o.caml_check_bound,a0=o.caml_equal,aY=o.caml_fresh_oo_id,bR=o.caml_ml_string_length,d=o.caml_new_string,aZ=o.caml_notequal,aX=o.caml_register_global,bQ=o.caml_string_get,J=o.caml_string_notequal,t=o.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):o.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):o.caml_call_gen(a,[b,c])}function
j(a,b,c,d){return a.length==3?a(b,c,d):o.caml_call_gen(a,[b,c,d])}function
s(a,b,c,d,e){return a.length==4?a(b,c,d,e):o.caml_call_gen(a,[b,c,d,e])}function
$(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):o.caml_call_gen(a,[b,c,d,e,f])}function
aC(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):o.caml_call_gen(a,[b,c,d,e,f,g])}function
gy(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):o.caml_call_gen(a,[b,c,d,e,f,g,h])}var
h=o.caml_get_global_data(),T=[0,[0,0,0]],bE=[0,d(a6),d(bX)],bI=d(a6),l=h.Tacmach,K=h.Printer,c=h.Pp,g=h.EConstr,n=h.Names,bF=h.Ltac_plugin__Tacenv,B=h.Genarg,an=h.Ltac_plugin__Tacinterp,am=h.Proofview,S=h.Context,i=h.Evd,v=h.DAst,m=h.CErrors,f=h.Constr,e=h.Util,bm=h.Typeclasses,ak=h.Assert_failure,aO=h.Evar,bn=h.UState,L=h.Ppconstr,H=h.Option,ae=h.Not_found,aR=h.Vars,aB=h.Goal,ag=h.Environ,ai=h.Evarutil,bd=h.Ftactic,M=h.Libnames,bh=h.Constrexpr_ops,as=h.Stdlib,bv=h.Termops,bk=h.Pretype_errors,bp=h.Recordops,aw=h.Term,aK=h.Evarconv,aL=h.Unification,N=h.CAst,W=h.Geninterp,be=h.Genintern,at=h.Global,ba=h.Constrintern,a9=h.Feedback,bG=h.Mltop,y=h.CLexer,q=h.Pcoq,ap=h.Ltac_plugin__Tacentries,e1=h.Tacticals,eM=h.Tactics,d_=h.Stdarg,d3=h.Ltac_plugin__Tacsubst,dM=h.Ltac_plugin__Tacintern,c_=h.CArray,c1=h.Failure,cW=h.Invalid_argument,cT=h.Sorts,cS=h.Globnames,cM=h.Reductionops,cx=h.Conv_oracle,cw=h.Reduction,ct=h.Glob_ops,ci=h.Ltac_plugin__Pptactic,b8=h.CamlinternalLazy,ca=h.Goptions;aX(156,[0,0,0],"Ssrmatching_plugin");var
eX=d("matches:"),eY=d("instance:"),e2=d("Not supported"),eV=[0,1],eW=[0,1],eZ=d("BEGIN INSTANCES"),e0=d("END INSTANCES"),eT=d(bX),eN=d(b1),eL=d("selected"),eF=d("matching impacts evars"),eD=d(" does not match any subterm of the goal"),eE=d(b4),eB=[0,1],ex=[0,[0,1,0]],ey=[0,[0,0,[0,1,0]]],ew=d("pattern without redex."),ev=[0,0],er=[0,d(ar),1135,48],en=d("in the pattern?"),eo=d('Does the variable bound by the "in" construct occur '),ep=d(" did not instantiate ?"),eq=d("Matching the pattern "),et=[0,1],eu=[0,1],es=[0,1],el=d("typed as: "),ek=d("decoded as: "),ej=[0,d(ar),1055,58],eg=d(bT),eh=d(a1),ei=d(ab),ef=[0,d(ar),1010,55],ed=d("."),ee=d("bad encoding for pattern "),ec=d("interpreting: "),ea=d("interpreting a term with no ist"),d9=[0,d(ar),942,12],d8=d(bV),dP=d(bV),dO=d(bT),dN=d("globbing pattern: "),dQ=d("( _ as _ )"),dR=d("( _ as _ in _ )"),dS=d("( _ in _ )"),dT=d("( _ in _ in _ )"),dU=d(ab),dW=d(ab),dX=d(ab),dZ=d(ab),dY=d("where are we?."),dV=d(ab),d0=d(a1),d1=d(a1),dC=d(aa),dD=d(u),dE=d(u),dF=d(aa),dG=d(u),dH=d(u),dI=d(u),dJ=d(a2),du=d(aa),dv=d(u),dw=d(u),dx=d(aa),dy=d(u),dz=d(u),dA=d(u),dB=d(a2),dl=d(aa),dm=d(u),dn=d(u),dp=d(aa),dq=d(u),dr=d(u),ds=d(u),dt=d(a2),dj=d("matches but type classes inference fails"),dk=d("does not match any subterm of the goal"),di=d(bU),dg=d("are equal to the "),dh=d("all matches of "),df=d("companion function never called."),c$=d("of "),da=d(" of the "),de=d(" of"),db=d(" occurrence"),dc=d(" < "),dd=d("Only "),c5=d(bW),c6=d(b0),c3=d(bW),c4=d(b0),c8=d("term "),c9=d(b4),c7=d(bU),c2=d(bZ),cZ=d(bZ),cV=d("incomplete ise in match_upats_FO."),cX=d("IN FO."),cR=[0,d(ar),461,13],cN=d(u),cO=d("indeterminate "),cP=d("indeterminate pattern"),cD=d("RHS"),cC=d("LHS"),cz=[0,0],cy=[0,1],cq=d("combineCG: different ist"),cr=d("have: mixed C-G constr."),cs=d("have: mixed G-C constr."),cn=[0,0],cm=[12,0,0,0],cl=d("not a GLambda"),cj=d("not a CRef."),ce=d("$"),cc=d(")"),cd=d(b3),cb=d("glob_constr: term with no ist"),b7=d("SSR: "),b6=[0,d("ssrmatching")],gx=d("SSRMATCHINGDEBUG"),b9=[0,d("Debug"),[0,d("SsrMatching"),0]],b_=d("ssrmatching debugging"),co=[13,0,0,0],cv=d("Ssrmatching_plugin.Ssrmatching.NoProgress"),cB=d("Ssrmatching_plugin.Ssrmatching.NoMatch"),cG=d(bS),cJ=d(bS),cU=d("Ssrmatching_plugin.Ssrmatching.FoundUnif"),dL=d("rpatternty"),eQ=d(b1),eU=d(a6),fZ=d(b3),f0=d("@"),fg=d(U),fk=d(U),fp=d(U),fs=d(U),fw=d(U),fz=d(U),fE=d(U),fH=d("as"),fJ=d("rpattern"),fV=d(bY),fX=d("cpattern"),f1=d("ssrtermkind"),gf=d(bY),gh=d("lcpattern"),gr=d("ssrpatternarg"),gu=d("ssrinstancesoftpat"),gw=d("ssrinstoftpat"),b5=m[5];function
D(a){return b(b5,a,b6)}function
a8(d,b){var
e=a(c[3],b);return j(m[5],d,[0,b],e)}var
aE=a9[6],ad=[0,function(a){return 0}];function
aF(d){var
e=o.caml_obj_tag(d),f=250===e?d[1]:ac===e?a(b8[2],d):d,g=a(c[3],b7),h=b(c[12],g,f);return b(a9[9],0,h)}try{o.caml_sys_getenv(gx);ad[1]=aF}catch(a){a=t(a);if(a!==ae)throw a}function
a_(a){return a?(ad[1]=aF,0):(ad[1]=function(a){return 0},0)}var
b$=[0,0,b_,b9,function(b){return a(e[3],ad)===aF?1:0},a_];b(ca[4],0,b$);function
af(b){return a(a(e[3],ad),b)}function
a$(c){var
b=a(f[30],c);return 9===b[0]?[0,b[1],b[2]]:[0,c,[0]]}function
bb(e,d,c){var
a=bQ(d,c);if(48<=a)var
b=61===a?1:123===a?1:0;else{if(40===a)return 0;var
b=47<=a?1:0}return b?1:40===e?1:0}function
bc(n,g,f){var
o=a(g,f),p=a(c[49],o),h=b(as[17],p,ce),d=0;for(;;){if(22<(bQ(h,d)-10|0)>>>0){if(b(n,h,d)){var
i=a(c[3],cc),j=a(g,f),k=a(c[3],cd),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[26],1,m)}return a(g,f)}var
d=b(e[4],d,1);continue}}var
cf=L[18],cg=L[17];function
aG(c){var
e=c[2],f=c[1],d=a(at[2],0),h=a(i[17],d);function
g(e){var
c=e[2],g=e[1];if(c)return j(cg,d,h,c[1]);var
f=a(at[2],0);return b(K[27],f,g)}return bc(function(a,b){return bb(f,a,b)},g,e)}function
p(c){var
e=c[2],f=c[1],d=a(at[2],0),h=a(i[17],d);function
g(e){var
c=e[2],g=e[1];if(c)return j(cf,d,h,c[1]);var
f=a(at[2],0);return b(K[26],f,g)}return bc(function(a,b){return bb(f,a,b)},g,e)}function
ch(e,g){var
c=a(B[2],e),f=a(W[1][1],e);function
h(b,a){return[0,b,a]}function
i(b,a){return a}function
j(c,b){return a(bd[1],[0,f,b])}function
d(c,a,f,e,d){return b(g,c,a)}b(be[9],c,h);b(be[10],c,i);b(W[7],c,j);b(W[4],c,[0,[0,f]]);s(ci[1],c,d,d,d);return c}function
aH(c){var
b=c[1];return 0===b[0]?a(M[32],b[1]):0}function
ck(c){var
b=a(v[1],c);if(5===b[0])if(b[1])return 1;return 0}function
aI(e){var
b=a(v[1],e);if(5===b[0]){var
d=b[1];if(d)return[0,d[1],b[4]]}var
f=a(c[3],cl);return j(m[2],0,0,f)}function
bf(b){return 13===a(v[1],b)[0]?1:0}function
bg(a){return b(N[1],a,cm)}function
au(d,c,a){return b(N[1],d,[16,c,[0,a]])}var
cp=v[3],O=function(a){return b(cp,0,a)}(co);function
ah(c,a){return b(v[3],0,[14,c,[0,a]])}function
aJ(e,d,o,n){function
f(e,b){if(e){var
d=e[1];if(b){if(d===b[1])return[0,d];var
f=a(c[3],cq);return j(m[2],0,0,f)}return[0,d]}return b?[0,b[1]]:0}var
g=e[2],h=g[2],i=e[1],p=g[1];if(h){var
k=d[2][2];if(k){var
q=k[1],r=h[1],s=f(e[3],d[3]);return[0,i,[0,O,[0,b(o,r,q)]],s]}var
t=a(c[3],cr);return j(m[2],0,0,t)}var
l=d[2];if(l[2]){var
u=a(c[3],cs);return j(m[2],0,0,u)}var
v=l[1],w=f(e[3],d[3]);return[0,i,[0,b(n,p,v),0],w]}function
P(d){var
b=d[2],c=b[2],e=b[1];return c?a(bh[5],c[1]):a(ct[25],e)}function
bi(c,b,a){return[0,c,[0,O,[0,b]],a]}var
cu=32;function
bj(a,b){return bi(cu,a,b)}function
z(d,c){var
e=a(g[9],c),f=b(ai[30],d,e);return a(g[r][1],f)}var
X=[a5,cv,aY(0)];function
aj(e,b,d,c){var
f=a(i[155],b),g=[0,a(i[47],b),f];try{aC(cw[14],0,0,e,[0,g],d,c);var
h=1;return h}catch(a){return 0}}function
Q(d,a,c,b){try{var
e=$(aK[4],0,d,a,c,b);return e}catch(a){a=t(a);if(a[1]===aK[3])throw[0,bk[1],d,a[2],[4,c,b,[0,a[3]]]];throw a}}function
bl(l,k,f,j,i){var
d=k,c=0,m=f.length-1;for(;;){if(c===m)return d;var
n=b(e[4],c,1),h=b(e[4],j,c),o=I(i,h)[1+h],p=a(g[9],o),q=I(f,c)[1+c],d=Q(l,d,a(g[9],q),p),c=n;continue}}function
aM(e,j,i,h){var
k=a(g[9],h),l=a(g[9],i),f=a(ag[5],e),c=a(cx[8],f),b=a(aL[4],c)[1],d=[0,0,b[2],b[3],b[4],c,b[6],b[7],b[8],b[9],b[10],1,1],m=[0,[0,d,d,d,0,a(aL[4],c)[5]]];return aC(aL[8],e,j,0,m,l,k)}function
aN(k,d,l){var
c=[0,k],m=a(g[r][1],l);function
h(l){var
m=a(f[30],l);if(3===m[0]){var
k=m[1];try{var
s=h(b(i[43],d,k));return s}catch(l){var
g=k[1],n=b(e[24][15],h,k[2]),o=a(e[3],c);if(1-b(i[26],o,g)){var
p=b(i[23],d,g),q=b(ai[38],d,p),r=a(e[3],c);c[1]=j(i[22],r,g,q)}return a(f[6],[0,g,n])}}return b(f[aD],h,l)}function
n(f,l,q){if(0===a(i[9],l)){var
m=b(i[23],d,f),k=a(i[9],m);if(k){var
n=h(a(g[r][1],k[1])),o=a(g[9],n),p=a(e[3],c);c[1]=j(i[31],f,o,p);return 0}return 0}return 0}var
o=h(m);j(i[27],n,k,0);var
p=a(g[9],o),q=a(i[b2],d);return[0,a(e[3],c),q,p]}function
av(k,j,h,r,q,p){var
t=k?k[1]:1,c=s(aK[7],j,0,0,r),u=a(i[54],c),d=aN(h,c,q),l=d[3],e=d[2],m=d[1],n=a(i[V],m);function
v(a){return b(i[36],n,a)}var
w=b(aO[7][18],v,u),x=b(i[53],n,w),f=b(i[a3],x,e),o=t?aC(bm[22],0,0,0,cy,j,f):f;if(a(p,c)){if(o===f)return[0,m,e,l];var
g=aN(h,o,l),y=g[3],z=g[1];return[0,z,b(bn[6],e,g[2]),y]}throw X}function
R(d,c,f,a){var
g=Q(d,c,f,a),e=av(cz,d,c,g,a,function(a){return 1});return b(i[a3],e[1],e[2])}function
cA(c,e,d){var
f=a(i[89],c),g=a(l[2],c),h=R(a(l[5],c),g,e,d);return b(l[3],f,h)}function
bo(c,k){var
d=a(f[30],k);switch(d[0]){case
3:return b(e[24][13],c,d[1][2]);case
5:var
l=d[3];a(c,d[1]);return a(c,l);case
8:var
n=d[4],o=d[3];a(c,d[2]);a(c,o);return a(c,n);case
9:var
p=d[2];a(c,d[1]);return b(e[24][13],c,p);case
13:var
q=d[4],r=d[2];a(c,d[3]);a(c,r);return b(e[24][13],c,q);case
16:return a(c,d[2]);case
6:case
7:var
m=d[3];a(c,d[2]);return a(c,m);case
14:case
15:var
h=d[1][2],i=h[2],s=h[3],j=b(e[5],i.length-1,1),t=0;if(!(j<0)){var
g=t;for(;;){a(c,I(i,g)[1+g]);a(c,I(s,g)[1+g]);var
u=g+1|0;if(j!==g){var
g=u;continue}break}}return 0;default:return 0}}var
x=[a5,cB,aY(0)];function
Y(b){return 0===b?a(c[3],cC):a(c[3],cD)}function
cE(a){return 0===a?1:0}function
E(b,a){return 1}function
cF(c){try{var
d=a(bp[5],[1,c]),f=b(e[4],1,d);return f}catch(a){return 0}}function
bq(b){switch(a(f[30],b)[0]){case
4:case
6:case
7:case
13:case
14:case
15:return 1;default:return 0}}var
cH=a(n[1][6],cG),cI=a(f[2],cH);function
F(g,e,d){function
c(d){return a(f[39],d)?cI:b(f[aD],c,d)}var
h=c(d);return j(K[6],g,e,h)}var
cK=a(n[1][6],cJ),cL=a(g[11],cK);function
aP(e,a,d){function
c(d){return b(g[55],a,d)?cL:j(g[110],a,c,d)}var
f=c(d);return j(K[11],e,a,f)}function
br(I,H,G,E,C,Z,X,W){var
u=C[1],_=C[2],$=H?H[1]:0,aa=a(g[9],W),J=b(cM[34],u,aa),ab=J[2],h=a(g[r][1],J[1]),d=b(e[22][68],g[r][1],ab),p=a(f[30],h);switch(p[0]){case
3:var
L=p[1][1];if(b(i[26],E,L))var
w=[0,[0,L],h,d];else
if(0===d)if(I)var
M=I[1],ad=M[1],ae=F(G,u,M[2]),af=a(c[3],cN),ah=Y(ad),aj=a(c[3],cO),ak=b(c[12],aj,ah),al=b(c[12],ak,af),am=b(c[12],al,ae),w=a(D(0),am);else
var
an=a(c[3],cP),w=j(m[5],0,0,an);else
var
w=[0,5,h,d];var
l=w,o=0;break;case
7:var
l=[0,3,h,d],o=0;break;case
8:var
ao=p[4],ap=p[2],aq=aZ(ao,a(f[1],1))?[0,2,h,d]:[0,5,ap,d],l=aq,o=0;break;case
10:var
N=p[1][1],A=cF(N);if(0===A)var
B=0;else
if(a(e[22][1],d)<A)var
B=0;else
var
P=b(e[22][107],A,d),ar=P[2],O=[0,[1,N],b(aw[13],h,P[1]),ar],B=1;if(!B)var
O=[0,1,h,d];var
l=O,o=0;break;case
16:var
l=[0,[1,a(n[68][6],p[1])],h,d],o=0;break;case
1:case
11:case
12:var
z=0,v=h,y=d,o=1;break;default:var
z=4,v=h,y=d,o=1}if(!o)var
z=l[1],v=l[2],y=l[3];var
K=a(e[24][12],y),x=[0,u],q=[0,u],ac=a(f[16],[0,v,K]),T=$?1:0,Q=a(ag[13],G),R=a(e[22][1],Q),U=b(e[4],R,T);function
k(p){var
c=p;for(;;){var
h=a(f[30],c);if(3===h[0]){var
d=h[1],l=d[1],u=d[2];try{var
O=a(e[3],q),P=k(b(i[43],O,d));return P}catch(h){h=t(h);if(h===i[41]){if(b(i[26],E,l))return b(f[aD],k,c);var
v=a(e[3],q),m=b(i[23],v,l),w=a(i[6],m),y=b(e[5],u.length-1,U),z=b(as[6],0,y),A=b(e[22][108],z,w),B=function(c,b){var
d=c[2],e=c[1];if(0===b[0]){var
g=b[1],h=k(b[2]),i=j(aw[5],g,h,d);return[0,[0,a(f[2],g[1]),e],i]}var
l=b[2],m=b[1],n=k(b[3]),o=k(l);return[0,e,s(aw[4],m,o,n,d)]},C=a(g[r][5],A),D=[0,0,k(a(g[r][1],m[1]))],n=j(S[11][9],B,D,C),F=n[2],G=n[1],o=a(ai[1],0),H=a(e[3],x),I=a(g[9],F);x[1]=s(i[118],o,I,0,H);var
J=a(e[3],q),K=a(f[5],o),L=b(aw[13],K,G),M=a(g[9],L);q[1]=j(i[31],l,M,J);var
N=a(e[3],q),c=b(i[43],N,d);continue}throw h}}return b(f[aD],k,c)}}var
V=k(ac);return[0,a(e[3],x),[0,z,V,v,K,_,X,Z]]}function
bs(g,d,e){var
h=d[1],n=d[3],o=d[2],j=a$(g),k=j[1],p=j[2],l=a(f[30],k);switch(l[0]){case
3:var
m=l[1][1];if(b(i[35],h,m))throw x;var
c=[0,m];break;case
7:var
c=3;break;case
8:var
c=2;break;case
10:var
c=1;break;case
1:case
11:case
12:var
c=0;break;default:var
c=4}return[0,h,o,[0,c,g,k,p,n,e[6],e[7]]]}function
cQ(h,k,j){function
i(b){var
c=a(bp[13],[0,[1,h],b])[2][7];return a(e[22][1],c)}function
l(c){var
b=a(f[30],c);switch(b[0]){case
9:return b[2].length-1;case
16:return 0;default:throw[0,ak,cR]}}try{var
g=a(f[30],k);switch(g[0]){case
4:var
d=i([1,a(cT[12],g[1])]),c=2;break;case
6:var
d=i(0),c=2;break;case
10:if(b(n[19][12],g[1][1],h))var
d=l(j[3]),c=2;else
var
c=1;break;case
16:var
m=a(n[68][6],g[1]);if(b(n[19][12],m,h))var
d=l(j[3]),c=2;else
var
c=0;break;case
1:case
11:case
12:var
c=1;break;default:var
c=0}switch(c){case
0:var
d=-1;break;case
1:var
d=i([0,a(cS[15],k)]);break}return d}catch(a){a=t(a);if(a===ae)return-1;throw a}}function
bt(d,c){var
b=a(f[30],c);return 3===b[0]?a0(d,b[1][1]):0}function
ax(d,c,b){if(0===c)return d;var
g=c===b.length-1?b:j(e[24][7],b,0,c);return a(f[16],[0,d,g])}function
aQ(j){function
h(l,k){var
d=l,c=k;for(;;){var
g=a(f[30],d);switch(g[0]){case
3:var
m=g[1];try{var
n=h(b(i[43],j,m),c);return n}catch(a){return[0,d,c]}case
5:var
d=g[1];continue;case
9:var
o=g[1],d=o,c=b(e[24][5],g[2],c);continue;default:return[0,d,c]}}}return function(b){var
c=a(f[30],b);switch(c[0]){case
9:return h(c[1],c[2]);case
3:case
5:return h(b,[0]);default:return[0,b,[0]]}}}var
al=[a5,cU,aY(0)];function
bu(c,a){var
d=b(i[105],c,a);return function(a){function
c(c){try{b(i[24],a,c);var
d=1;return d}catch(a){a=t(a);if(a===ae)return 0;throw a}}return b(aO[7][16],c,d)}}function
cY(D,l,k,h,i,d){var
x=[0,0],y=[0,0],E=bu(h,a(g[9],d));function
c(l,h,s,i,q){var
p=a(aQ(i),q),n=p[2],d=p[1],k=[0,-1],o=n.length-1,u=0;function
v(i,l){var
h=i[4].length-1;if(o<h)return l;var
j=i[1];if(typeof
j==="number")switch(j){case
0:if(b(f[86],i[3],d))var
g=h,c=1;else
var
c=0;break;case
1:if(b(f[86],i[3],d))var
g=h,c=1;else
var
c=0;break;case
2:if(a(f[46],d))var
g=h,c=1;else
var
c=0;break;case
3:if(a(f[45],d))var
g=h,c=1;else
var
c=0;break;case
4:if(bq(d))var
g=h,c=1;else
var
c=0;break;default:var
g=h,c=1}else
if(0===j[0])if(bt(j[1],d))var
g=h,c=1;else
var
c=0;else
var
n=cQ(j[1],d,i),m=b(e[4],h,n),p=o<m?-1:m,g=p,c=1;if(!c)var
g=-1;if(g<h)return l;if(a(e[3],k)<g)k[1]=o;return[0,[0,i,g],l]}var
w=j(e[22][16],v,l,u);for(;;){if(0<=a(e[3],k)){var
z=a(e[3],k);k[1]=-1;var
A=function(j){return function(z){var
p=z[2],c=z[1];if(j<=p)var
u=j<p?1:0;else
if(5===c[1]){k[1]=b(e[5],j,1);var
u=0}else{if(a(e[3],k)<p)k[1]=p;var
u=1}if(u)return 0;try{var
v=c[1];if(typeof
v==="number")switch(v){case
2:var
q=a(f[68],d),M=q[4],N=q[3],O=q[2],P=q[1],C=a(f[68],c[3]),R=C[4],S=C[2],T=a(g[9],O),U=Q(h,i,a(g[9],S),T),V=a(g[9],M),W=a(g[9],R),o=Q(b(ag[28],[0,P,N],h),U,W,V),l=1;break;case
5:var
l=0;break;case
3:case
4:var
Y=a(g[9],d),o=Q(h,i,a(g[9],c[3]),Y),l=1;break;default:var
o=i,l=1}else
if(0===v[0])var
$=a(f[73],c[3])[2],o=bl(h,i,$,0,a(f[73],d)[2]),l=1;else
var
l=0;if(!l)var
Z=ax(d,b(e[5],j,c[4].length-1),n),_=a(g[9],Z),o=Q(h,i,a(g[9],c[3]),_);var
F=b(e[5],j,c[4].length-1),G=bl(h,o,c[4],F,n),B=ax(d,j,n),H=a(c[7],B),w=av(0,h,s,G,a(g[9],c[5]),H),I=a(e[14],w),J=a(g[r][1],I),K=a(e[13],w),L=a(D,bs(B,[0,a(e[12],w),K,J],c));return L}catch(b){b=t(b);if(b[1]===al){if(a(E,b[2][1]))throw b}else{if(b===X){x[1]=1;return 0}if(b[1]===bk[1]){var
A=b[4];if(typeof
A!=="number"&&19===A[0]){y[1]=1;return 0}}}if(a(m[13],b))return 0;throw b}}}(z);b(e[22][11],A,w);continue}bo(function(a){return c(l,h,s,i,a)},d);var
B=function(a){return c(l,h,s,i,a)};return b(e[24][13],B,n)}}c(l,k,h,i,d);if(a(e[3],x))throw X;return a(e[3],y)}function
aS(b,c){return a(e[3],b)?0:(b[1]=[0,a(c,0)],0)}function
aT(d){var
b=a(e[3],d);if(b)return b[1];var
f=a(c[3],cZ);return j(m[2],0,0,f)}function
c0(f){var
g=a(e[3],f);if(g){var
d=g[1],h=d[3],i=d[2],k=d[1];f[1]=[0,[0,k,b(e[4],i,1),h]];try{var
l=b(e[22][7],h,i);return l}catch(a){a=t(a);if(a[1]===c1)throw x;throw a}}var
n=a(c[3],c2);return j(m[2],0,0,n)}function
A(K,E,q,G,y,w){var
k=w[2],i=w[1],A=K?K[1]:0,H=E?E[1]:0,l=[0,0],B=[0,0];if(y){var
h=y[1];if(0===h[1])var
L=h[2],p=0!==L?1:0,d=L;else
var
R=h[2],p=0===R?1:0,d=R}else
var
p=0,d=0;var
u=j(e[22][16],as[6],d,0),M=o.caml_make_vect(u,1-p);function
T(c){var
a=b(e[5],c,1);I(M,a)[1+a]=p;return 0}b(e[22][11],T,d);if(0===u)B[1]=p;var
C=[0,0];function
N(b){return a(f[16],[0,b[3],b[4]])}function
J(d){if(q){var
l=q[1],n=l[2],o=l[1];if(k)if(!k[2]){var
A=k[1],B=a(c[5],0),C=F(d,i,N(A)),D=a(c[6],4),E=a(c[5],0),G=F(d,i,n),H=a(c[3],c5),I=Y(o),J=a(c[3],c6),K=b(c[12],J,I),L=b(c[12],K,H),M=b(c[12],L,G),O=b(c[12],M,E),P=b(c[12],O,D),Q=b(c[12],P,C);return b(c[12],Q,B)}var
s=a(c[13],0),t=F(d,i,n),u=a(c[3],c3),v=Y(o),w=a(c[3],c4),x=b(c[12],w,v),y=b(c[12],x,u),z=b(c[12],y,t);return b(c[12],z,s)}if(k)if(!k[2]){var
e=k[1],S=a(c[13],0),T=F(d,i,N(e)),h=e[1];if(typeof
h==="number")if(5<=h)var
f=0;else
var
r=a(g[9],e[5]),p=1-b(bv[27],i,r),f=1;else
var
f=0;if(!f)var
p=0;var
U=p?a(c[3],c8):a(c[3],c9),V=b(c[12],U,T);return b(c[12],V,S)}var
R=a(c[3],c7);return j(m[2],0,0,R)}var
v=[0,0];function
O(b){return a(e[3],v)}function
V(c){if(A){var
d=a(e[3],v);v[1]=b(e[23],d,[0,c,0]);return 0}throw[0,al,c]}function
P(a){if(a){var
c=a[1],d=c[3],g=c[1],h=a[2],i=d[4],k=d[3],l=z(g,d[5]),m=z(g,k),n=function(a){return z(g,a)},o=b(e[24][15],n,i),p=function(d){var
a=d[3],c=d[1],k=a[4],n=a[3],p=z(c,a[5]),q=z(c,n);function
r(a){return z(c,a)}var
s=b(e[24][15],r,k),g=b(f[81],l,p);if(g)var
h=b(f[81],m,q),i=h?j(c_[37],f[81],o,s):h;else
var
i=g;return 1-i};return[0,c,P(b(e[22][61],p,h))]}return 0}function
U(am){var
r=a(e[3],C);if(r)var
s=r[1],B=s[1],h=a(e[22][5],s[3]),i=h[3],g=B,d=h[1],w=h[2],v=i[3],t=i[4],k=i;else{if(H)throw x;var
al=a(c[3],df),A=j(m[2],0,0,al),o=A[2],p=o[3],g=A[1],d=o[1],w=o[2],v=p[3],t=p[4],k=p}var
n=a(f[16],[0,v,t]);if(u<=a(e[3],l))return[0,n,k[6],[0,d,w,k[5]]];if(q)var
y=q[1],E=y[1],G=F(g,d,y[2]),I=a(c[3],c$),J=a(c[5],0),K=F(g,d,n),L=a(c[6],4),M=a(c[5],0),N=Y(E),O=a(c[3],da),P=b(c[12],O,N),Q=b(c[12],P,M),R=b(c[12],Q,L),S=b(c[12],R,K),T=b(c[12],S,J),U=b(c[12],T,I),z=b(c[12],U,G);else
var
ah=F(g,d,n),ai=a(c[13],0),aj=a(c[3],de),ak=b(c[12],aj,ai),z=b(c[12],ak,ah);var
V=a(e[3],l),W=b(e[20][46],V,db),X=a(c[3],W),Z=a(c[16],u),_=a(c[3],dc),$=a(e[3],l),aa=a(c[16],$),ab=a(c[3],dd),ac=b(c[12],ab,aa),ad=b(c[12],ac,_),ae=b(c[12],ad,Z),af=b(c[12],ae,X),ag=b(c[12],af,z);return a(D(0),ag)}return[0,function(d,w,Z,W){aS(C,function(U){var
h=[0,0];try{if(1-A){var
C=bu(G,a(g[9],w)),p=function(v){var
q=a(aQ(i),v),o=q[2],h=q[1],l=[0,-1],s=o.length-1,w=0;function
y(g,j){var
k=a(f[30],g[2]),i=9===k[0]?k[2].length-1:0;if(s<i)return j;var
d=g[1];if(typeof
d==="number")switch(d){case
0:var
c=b(f[86],g[3],h);break;case
1:var
c=b(f[86],g[3],h);break;case
2:var
c=a(f[46],h);break;case
3:var
c=a(f[45],h);break;case
4:var
c=bq(h);break;default:l[1]=s;var
c=1}else
if(0===d[0])var
c=bt(d[1],h);else{var
o=d[1],u=a(f[18],o),p=b(f[81],h,u);if(p)var
q=p;else{var
m=a(f[30],h);if(16===m[0])var
t=a(n[68][6],m[1]),r=b(n[19][12],t,o);else
var
r=0;var
q=r}var
c=q}if(c){if(a(e[3],l)<i)l[1]=i;return[0,[0,g,i],j]}return j}var
z=j(e[22][16],y,k,w);for(;;){if(0<=a(e[3],l)){var
u=a(e[3],l);l[1]=-1;var
A=ax(h,u,o),B=function(q,n){return function(y){var
p=y[2],k=y[1];if(q<=p)var
u=q<p?1:0;else
if(5===k[1]){l[1]=b(e[5],q,1);var
u=0}else{if(a(e[3],l)<p)l[1]=p;var
u=1}if(!u)if(a(aR[2],n))try{var
v=k[1];if(typeof
v==="number")if(2===v){var
z=function(g){var
d=a$(g),h=d[2],c=a(f[68],d[1]),i=c[4],j=c[3],k=c[1],l=b(e[24][45],c[2],h),m=[0,a(f[14],[0,k,j,i]),l];return a(f[16],m)},E=z(n);aM(d,i,z(k[2]),E);var
s=1}else
if(5<=v){var
B=function(c){var
d=f[9],e=[0,b(S[4],0,0),d,c];return a(f[14],e)},N=B(n);aM(d,i,B(k[2]),N);var
s=1}else
var
s=0;else
var
s=0;if(!s)aM(d,i,k[2],n);var
F=a(f[16],[0,k[3],k[4]]);try{var
H=a(g[9],n),I=Q(d,i,a(g[9],F),H)}catch(b){b=t(b);if(a(m[13],b))throw x;throw b}var
A=ax(h,q,o),J=a(k[7],A),w=av(0,d,G,I,a(g[9],k[5]),J),K=a(e[14],w),L=a(g[r][1],K),M=a(e[13],w);throw[0,al,bs(A,[0,a(e[12],w),M,L],k)]}catch(b){b=t(b);if(b[1]===al){if(a(C,b[2][1]))throw b}else
if(b===ae){var
D=a(c[3],cV);return j(m[2],0,0,D)}if(a(m[13],b))return 0;throw b}return 0}}(u,A);b(e[22][11],B,z);continue}bo(p,h);return b(e[24][13],p,o)}};try{p(w)}catch(b){b=t(b);if(b[1]!==cW)throw b;var
o=a(c[3],cX);j(m[2],0,0,o)}}h[1]=cY(V,k,d,G,i,w);throw x}catch(f){f=t(f);if(f[1]===al)return[0,d,0,[0,f[2],0]];var
T=f===x?0:f===X?0:1;if(!T)if(A)if(0!==O(0))return[0,d,0,P(O(0))];if(f===x)if(!H){if(a(e[3],h)){var
I=a(c[22],dj),K=J(d),L=b(c[12],K,I);return a(D(0),L)}var
M=a(c[3],dk),N=J(d),R=b(c[12],N,M);return a(D(0),R)}if(f===X){if(H)throw x;if(q)var
l=q[1][1];else
var
F=a(c[3],di),l=j(m[2],0,0,F);var
s=Y(cE(l)),u=a(c[3],dg),v=J(d),y=a(c[3],dh),z=b(c[12],y,v),B=b(c[12],z,u),E=b(c[12],B,s);return a(D(0),E)}throw f}});if(A)var
K=c0(C);else
var
aa=aT(C),ab=a(e[14],aa),K=a(e[22][5],ab);var
h=K[3],E=h[4],o=K[1];if(a(e[3],B))return w;var
L=h[1];if(typeof
L==="number")switch(L){case
0:var
y=a(f[86],h[3]),v=1;break;case
1:var
y=a(f[86],h[3]),v=1;break;case
2:var
z=a(f[68],h[3]),N=z[4],R=z[2],T=b(ag[28],[0,z[1],z[3]],d),y=function(e){var
b=a(f[30],e);if(8===b[0]){var
g=b[4],c=aj(d,o,R,b[2]);return c?aj(T,o,N,g):c}return 0},v=1;break;case
3:var
y=function(b){return 7===a(f[30],b)[0]?aj(d,o,h[3],b):0},v=1;break;default:var
v=0}else
var
v=0;if(!v)var
U=h[3],y=function(a){return aj(d,o,U,a)};var
_=E.length-1;function
F(d,t){var
v=d[1],H=d[2];if(a(e[3],B))return t;var
w=a(aQ(o),t),i=w[2],j=w[1];if(_<=i.length-1)if(a(y,j)){var
c=0,C=E.length-1;for(;;){var
k=c===C?1:0;if(k)var
m=k;else{var
D=I(i,c)[1+c],n=aj(v,o,I(E,c)[1+c],D);if(n){var
c=b(e[4],c,1);continue}var
m=n}if(m){var
x=b(e[24][58],E.length-1,i),J=x[2],z=a(f[16],[0,j,x[1]]);l[1]++;if(a(e[3],l)===u)B[1]=p;if(a(e[3],l)<=u)var
G=a(e[3],l),q=b(e[5],G,1),A=I(M,q)[1+q];else
var
A=1-p;var
K=A?s(W,v,h[5],z,H):z,L=function(a){return F(d,a)},N=[0,K,b(e[24][62],L,J)];return a(f[16],N)}break}}function
O(a,c){var
d=c[2],f=c[1],h=0===a[0]?a:[0,a[1],a[3]],i=b(e[4],d,1);return[0,b(g[122],h,f),i]}function
P(c,b){var
d=F(c,a(g[r][1],b));return a(g[9],d)}var
Q=a(g[9],j),R=$(bv[18],o,O,P,d,Q),S=a(g[r][1],R);function
T(a){return F(d,a)}var
U=[0,S,b(e[24][62],T,i)];return a(f[16],U)}return F([0,d,Z],w)},U]}function
ay(d){switch(d[0]){case
0:return p(d[1]);case
1:var
e=p(d[1]),f=a(c[3],dl);return b(c[12],f,e);case
2:var
g=d[1],h=p(d[2]),i=a(c[3],dm),j=p(g),k=b(c[12],j,i);return b(c[12],k,h);case
3:var
l=d[1],m=p(d[2]),n=a(c[3],dn),o=p(l),q=a(c[3],dp),r=b(c[12],q,o),s=b(c[12],r,n);return b(c[12],s,m);case
4:var
t=d[2],u=d[1],v=p(d[3]),w=a(c[3],dq),x=p(t),y=a(c[3],dr),z=p(u),A=b(c[12],z,y),B=b(c[12],A,x),C=b(c[12],B,w);return b(c[12],C,v);default:var
D=d[2],E=d[1],F=p(d[3]),G=a(c[3],ds),H=p(D),I=a(c[3],dt),J=p(E),K=b(c[12],J,I),L=b(c[12],K,H),M=b(c[12],L,G);return b(c[12],M,F)}}function
bw(d){switch(d[0]){case
0:return p(d[1]);case
1:var
e=p(d[1]),f=a(c[3],du);return b(c[12],f,e);case
2:var
g=d[1],h=p(d[2]),i=a(c[3],dv),j=a(L[6],g),k=b(c[12],j,i);return b(c[12],k,h);case
3:var
l=d[1],m=p(d[2]),n=a(c[3],dw),o=a(L[6],l),q=a(c[3],dx),r=b(c[12],q,o),s=b(c[12],r,n);return b(c[12],s,m);case
4:var
t=d[2],u=d[1],v=p(d[3]),w=a(c[3],dy),x=a(L[6],t),y=a(c[3],dz),z=p(u),A=b(c[12],z,y),B=b(c[12],A,x),C=b(c[12],B,w);return b(c[12],C,v);default:var
D=d[2],E=d[1],F=p(d[3]),G=a(c[3],dA),H=a(L[6],D),I=a(c[3],dB),J=p(E),K=b(c[12],J,I),M=b(c[12],K,H),N=b(c[12],M,G);return b(c[12],N,F)}}function
dK(R,i){var
d=i[2],h=i[1];function
f(b){var
c=aN(h,h,a(g[9],b));return aP(R,h,a(e[14],c))}switch(d[0]){case
0:return f(d[1]);case
1:var
j=f(d[1]),k=a(c[3],dC);return b(c[12],k,j);case
2:var
l=d[1],m=f(d[2]),n=a(c[3],dD),o=f(l),p=b(c[12],o,n);return b(c[12],p,m);case
3:var
q=d[1],r=f(d[2]),s=a(c[3],dE),t=f(q),u=a(c[3],dF),v=b(c[12],u,t),w=b(c[12],v,s);return b(c[12],w,r);case
4:var
x=d[2],y=d[1],z=f(d[3]),A=a(c[3],dG),B=f(x),C=a(c[3],dH),D=f(y),E=b(c[12],D,C),F=b(c[12],E,B),G=b(c[12],F,A);return b(c[12],G,z);default:var
H=d[2],I=d[1],J=f(d[3]),K=a(c[3],dI),L=f(H),M=a(c[3],dJ),N=f(I),O=b(c[12],N,M),P=b(c[12],O,L),Q=b(c[12],P,K);return b(c[12],Q,J)}}var
aU=ch(dL,function(b,a){return ay});function
C(e,a){var
c=a[2][2],f=a[1];if(c)if(!a[3]){var
d=c[1];return[0,f,[0,b(dM[6],e,d)[1],[0,d]],0]}return a}function
bx(x,i){af([ac,function(f){var
d=aG(i),e=a(c[3],dN);return b(c[12],e,d)}]);function
f(b){var
c=C(x,bj(b,0));return a(e[13],c)}function
g(e,d,c){var
f=b(as[17],dO,d),g=[0,a(n[1][6],f)],h=0,i=0,j=0===c?O:b(v[3],0,[4,O,c]);return[0,e,[0,ah(O,b(v[3],0,[5,g,0,O,j])),i],h]}function
l(o,n){var
h=bg(0),e=o[1];if(0===e[0]){var
g=e[1];if(a(M[32],g))var
i=a(M[34],g),d=1;else
var
d=0}else
var
d=0;if(!d)var
k=a(c[3],cj),i=j(m[2],0,0,k);var
l=[4,[0,[0,[0,b(N[1],0,[0,i]),0],cn,h],0],n];return f(au(0,h,b(N[1],0,l)))[1]}function
Q(b){var
c=1-aH(b);return c?a8(a(bh[5],b),dP):c}var
R=i[2],S=R[2],d=i[1],Z=R[1];if(S){var
T=S[1];if(a4===d)return C(x,[0,40,[0,Z,[0,T]],0]);var
h=T[1];if(17===h[0]){var
U=h[1];if(!U[1]){var
o=U[2];if(J(o,dQ))if(J(o,dR))if(J(o,dS)){if(!J(o,dT)){var
p=h[2],y=p[1];if(y){var
z=y[2];if(z){var
A=z[2];if(A)if(!A[2])if(!p[2])if(!p[3])if(!p[4]){var
V=z[1],_=A[1],$=y[1];Q(V);var
aa=[0,l(V,_),0];return g(d,dU,[0,f($)[1],aa])}}}}}else{var
q=h[2],B=q[1];if(B){var
D=B[2];if(D)if(!D[2])if(!q[2])if(!q[3])if(!q[4]){var
E=D[1],k=B[1];try{var
W=f(k),r=f(E),F=W[1];if(W[2])if(r[2])var
X=r[1],ab=aH(k)?g(d,dW,[0,F,[0,X,[0,l(k,E),0]]]):g(d,dX,[0,F,[0,X,0]]),G=ab,w=1;else
var
w=0;else
if(r[2])var
w=0;else
var
G=g(d,dZ,[0,F,[0,r[1],0]]),w=1;if(!w)var
ad=a(c[3],dY),G=j(m[2],0,0,ad);return G}catch(a){a=t(a);if(aH(k))return g(d,dV,[0,l(k,E),0]);throw a}}}}else{var
s=h[2],H=s[1];if(H){var
I=H[2];if(I){var
K=I[2];if(K)if(!K[2])if(!s[2])if(!s[3])if(!s[4]){var
Y=I[1],ae=K[1],ag=H[1];Q(Y);var
ai=[0,l(Y,ae),0];return g(d,d0,[0,f(ag)[1],ai])}}}}else{var
u=h[2],L=u[1];if(L){var
P=L[2];if(P)if(!P[2])if(!u[2])if(!u[3])if(!u[4]){var
aj=L[1],ak=[0,f(P[1])[1],0];return g(d,d1,[0,f(aj)[1],ak])}}}}}return C(x,i)}return i}function
d2(b,a){switch(a[0]){case
0:return[0,bx(b,a[1])];case
1:return[1,C(b,a[1])];case
2:var
c=a[1];return[2,c,C(b,a[2])];case
3:var
d=a[1];return[3,d,C(b,a[2])];case
4:var
e=a[2],f=a[1],g=C(b,a[3]);return[4,C(b,f),e,g];default:var
h=a[2],i=a[1],j=C(b,a[3]);return[5,C(b,i),h,j]}}function
G(c,a){var
d=a[3],e=a[1];return[0,e,b(d3[3],c,a[2]),d]}function
d4(b,a){switch(a[0]){case
0:return[0,G(b,a[1])];case
1:return[1,G(b,a[1])];case
2:var
c=a[1];return[2,c,G(b,a[2])];case
3:var
d=a[1];return[3,d,G(b,a[2])];case
4:var
e=a[2],f=a[1],g=G(b,a[3]);return[4,G(b,f),e,g];default:var
h=a[2],i=a[1],j=G(b,a[3]);return[5,G(b,i),h,j]}}function
w(b,a){return[0,a[1],a[2],[0,b]]}function
d5(c,r,b){switch(b[0]){case
0:var
d=[0,w(c,b[1])];break;case
1:var
d=[1,w(c,b[1])];break;case
2:var
e=b[1],f=w(c,b[2]),d=[2,w(c,e),f];break;case
3:var
g=b[1],h=w(c,b[2]),d=[3,w(c,g),h];break;case
4:var
i=b[2],j=b[1],k=w(c,b[3]),m=w(c,i),d=[4,w(c,j),m,k];break;default:var
n=b[2],o=b[1],p=w(c,b[3]),q=w(c,n),d=[5,w(c,o),q,p]}return[0,a(l[2],r),d]}var
d6=e[12];function
d7(a,b){return[0,a[1],a[2],[0,b]]}function
az(k){var
e=k[2],f=e[2],g=a(v[1],e[1]);if(f){var
d=f[1][1];switch(d[0]){case
0:var
h=d[1];if(a(M[32],h))var
c=[0,a(M[34],h)],b=1;else
var
b=0;break;case
6:if(d[2])var
b=0;else{var
i=d[1][2];if(a(M[32],i))var
c=[0,a(M[34],i)],b=1;else
var
b=0}break;default:var
b=0}}else
if(0===g[0]){var
j=g[1];if(0===j[0])var
c=[0,j[1]],b=1;else
var
b=0}else
var
b=0;if(!b)var
c=0;return c?c[1]:a8(P(k),d8)}function
aA(g,f){var
h=f[3],i=f[2],u=a(l[2],g),v=a(l[5],g),d=i[2],k=i[1];if(d){var
o=d[1];if(h){var
p=n[1][10][1],q=h[1][1],r=function(c,d,a){return b(n[1][10][4],c,a)},s=j(n[1][11][12],r,q,p),e=ba[4];return gy(ba[7],1,v,u,0,0,[0,[0,s,e[2],e[3]]],o)}var
t=a(c[3],cb);return j(m[2],0,0,t)}return k}function
d$(d,c,b){var
e=w(d,b);return[0,a(l[2],c),e]}function
ao(u,i){var
j=i[3],v=i[2];if(j){var
h=d_[13],w=j[1],o=a(B[5],h),p=b(B[7],o,v),d=[0,0],q=b(an[10],w,p),k=function(b){d[1]=[0,b];return a(am[16],0)},l=b(bd[4],q,k),m=a(a(am[71][7],l),u)[2],f=a(e[3],d);if(f){var
n=f[1],s=a(B[6],h),t=[0,m,b(an[2][7],s,n)];return b(e[7],g[r][1],t)}throw[0,ak,d9]}var
x=a(c[3],ea);return a(D(0),x)}function
eb(l,d,c){var
m=a(n[1][10][5],l),e=b(aB[3][1],d,c),o=b(aB[3][4],d,c),k=a(i[121],d);try{var
p=a(ag[14],e),q=[0,$(ai[52],e,k,p,o,m)],f=q}catch(a){a=t(a);if(a[1]!==ai[50])throw a;var
f=0}if(f){var
g=f[1],h=j(aB[3][5],g[1],g[2],g[3]);return $(aB[3][7],e,h[3],c,h[1],h[2])}return k}function
by(E,h,D,C){af([ac,function(f){var
d=ay(D),e=a(c[3],ec);return b(c[12],e,d)}]);function
u(b,a){return[2,b,a]}function
al(b,a){return[3,b,a]}function
am(a){return[1,a]}function
k(a,c,b){var
d=a?a[1]:32;return[0,d,[0,c,0],b]}function
q(d,p,y,o){var
e=d[3];try{var
z=aA(h,d),f=a(v[1],z);switch(f[0]){case
1:var
q=f[1];if(a(H[2],e)){var
A=a(H[7],e)[1],r=b(n[1][11][3],q,A);if(r)var
s=1-a(H[3],p),u=s?1-a(H[3],E):s;else
var
u=r;if(u)var
C=a(H[7],e)[1],D=b(n[1][11][23],q,C),F=a(H[7],E),G=a(B[6],F),I=b(an[2][7],G,D),i=b(H[7],p,I),c=1,k=0;else
var
k=1}else
var
k=1;if(k)var
c=0;break;case
14:var
j=f[2];if(typeof
j==="number")var
g=1;else
if(0===j[0]){var
w=j[1];if(bf(f[1]))if(ck(w))var
x=aI(w),i=b(y,x[1],[0,32,[0,x[2],0],e]),c=1,g=0,l=0;else
var
l=1;else
var
l=1;if(l)var
c=0,g=0}else
var
g=1;if(g)var
c=0;break;default:var
c=0}if(!c)var
i=a(o,d);return i}catch(b){b=t(b);if(a(m[13],b))return a(o,d);throw b}}function
r(d,c,b,a){return q(k(0,c,d),0,b,a)}function
y(d,k){var
e=a(c[3],ed),f=a(c[3],d),g=a(c[3],ee),h=b(c[12],g,f),i=b(c[12],h,e);return j(m[2],0,0,i)}function
F(s,k,r,c){var
m=a(f[30],s);if(3===m[0]){var
u=m[1][1],n=a(l[6],h),o=a(S[11][4],n),d=[0,0];try{b(S[11][5],k,n);var
y=function(m){var
f=0===a(e[3],d)?1:0;if(f){var
n=b(i[23],c,m),g=a(i[5],n),h=a(S[11][4],g),j=o<h?1:0;if(j){var
p=b(e[5],h,o),q=b(e[5],p,1),r=b(e[22][7],g,q);d[1]=[0,a(S[11][1][2],r)];var
k=0}else
var
k=j;var
l=k}else
var
l=f;return l},g=d,p=y}catch(a){a=t(a);if(a!==ae)throw a;var
g=[0,[0,k]],p=function(a){return 0}}var
v=a(l[2],h),q=function(d,g){var
h=a(f[30],g);if(3===h[0]){var
c=h[1][1];if(!a0(c,u))if(!b(e[22][25],c,d))if(!b(i[26],v,c)){p(c);return[0,c,d]}return d}return j(f[100],q,d,g)},w=q(0,z(c,r)),x=function(c,d){if(b(i[35],c,d))return c;var
h=a(e[3],g);if(a(H[3],h))return c;var
j=a(e[3],g),f=a(H[7],j);af([ac,function(b){return a(L[6],f)}]);return eb(f,c,d)};return j(e[22][15],x,c,w)}throw[0,ak,ef]}function
A(c){switch(c[0]){case
0:var
g=c[1],z=g[2],B=z[1],L=g[1];if(z[2])return q(g,[0,A],u,function(a){return[0,a]});var
d=g[3],i=a(v[1],B);if(14===i[0]){var
l=i[2];if(typeof
l==="number")var
K=0;else
if(0===l[0]){var
C=l[1];if(bf(i[1])){var
N=aI(C)[1],D=a(n[1][8],N),E=8<bR(D)?1:0,O=E?o.caml_string_equal(j(e[20][4],D,0,8),eg):E;if(O){var
F=aI(C),P=F[2],f=a(n[1][8],F[1]),Q=b(e[5],bR(f),8),G=j(e[20][4],f,8,Q),h=a(v[1],P);if(J(G,eh)){if(!J(G,ei))if(4===h[0]){var
m=h[2];if(m){var
p=m[2],s=m[1];if(!p)return r(d,s,u,function(a){return[0,a]});var
t=p[2],H=p[1];if(!t){var
U=function(a){return y(f,a)},V=k(0,s,d);return r(d,H,function(a,b){return[4,V,a,b]},U)}if(!t[2]){var
R=t[1],S=function(a){return r(d,R,u,function(a){throw[0,ak,ej]})},T=k(0,s,d);return r(d,H,function(a,b){return[4,T,a,b]},S)}}}}else
if(4===h[0]){var
w=h[2];if(w){var
x=w[2];if(x)if(!x[2]){var
W=x[1],X=w[1],Y=function(a){return y(f,a)},Z=k(0,X,d);return r(d,W,function(a,b){return[5,Z,a,b]},Y)}}}return y(f,0)}}var
K=1}else
var
K=0}var
M=function(a){return[0,a]};return q(k([0,L],B,d),[0,A],u,M);case
1:return q(c[1],0,al,am);case
2:var
I=c[1],_=c[2],$=function(a){return[2,az(I),a]};return q(_,0,function(a,b){return[4,I,a,b]},$);case
3:var
aa=c[2];return[3,az(c[1]),aa];case
4:var
ab=c[3],ac=c[1];return[4,ac,az(c[2]),ab];default:var
ad=c[3],ae=c[1];return[5,ae,az(c[2]),ad]}}var
d=A(D);af([ac,function(g){var
e=bw(d),f=a(c[3],ek);return b(c[12],f,e)}]);if(C){var
G=C[1],p=[0,32,G[1],[0,G[2]]];switch(d[0]){case
0:var
I=d[1],ap=P(I),s=[0,aJ(I,p,function(a,b){return au(ap,a,b)},ah)];break;case
2:var
aD=d[2],aE=d[1],aF=aA(h,p),aG=p[3],s=[5,k(0,ah(O,aF),aG),aE,aD];break;case
4:var
ai=d[3],aH=d[2],aK=d[1],aL=p[3],aM=k(0,aA(h,p),aL),aN=P(ai),s=[4,aJ(aK,aM,function(a,b){return au(aN,a,b)},ah),aH,ai];break;case
5:var
aj=d[3],aO=d[2],aP=d[1],aQ=p[3],aS=k(0,aA(h,p),aQ),aT=P(aj),s=[5,aJ(aP,aS,function(a,b){return au(aT,a,b)},ah),aO,aj];break;default:var
s=d}var
g=s}else
var
g=d;af([ac,function(f){var
d=bw(g),e=a(c[3],el);return b(c[12],e,d)}]);function
K(a,d,c){var
e=c[3],f=c[2],g=f[2],h=f[1],i=c[1];if(g){var
k=g[1],l=bg(a),j=[5,b(N[1],a,d),l,0,k];return[0,i,[0,h,[0,b(N[1],a,j)]],e]}var
m=[7,d,b(v[3],a,[13,[1,d],0,0]),0,h];return[0,i,[0,b(v[3],a,m),0],e]}switch(g[0]){case
0:var
M=ao(h,g[1]);return[0,M[1],[0,M[2]]];case
1:var
Q=ao(h,g[1]);return[0,Q[1],[1,Q[2]]];case
2:case
3:var
R=g[1],T=ao(h,K(0,[0,R],g[2])),aq=T[1],U=a(f[68],T[2]),V=U[4],w=U[2],W=F(w,R,V,aq),ar=z(W,V),X=b(aR[14],w,ar),as=2===g[0]?[2,w,X]:[3,w,X];return[0,W,as];default:var
Y=g[2],at=g[1],Z=ao(h,K(0,[0,Y],g[3])),av=Z[1],_=a(f[68],Z[2]),$=_[4],x=_[2],aa=F(x,Y,$,av),aw=z(aa,$),ab=b(aR[14],x,aw),ax=a(i[89],h),ad=ao(b(l[3],ax,aa),at),ag=ad[2],aB=ad[1],aC=4===g[0]?[4,ag,x,ab]:[5,ag,x,ab];return[0,aB,aC]}}function
bz(c,b,a){return by(0,c,[0,b],a)}function
em(d){var
b=d[2];if(0===b[0]){var
c=a(f[30],b[1]);return 1===c[0]?[0,c[1]]:0}return 0}function
bA(w,e,k,o,n,q,p){function
h(b,a){return z(b,a)}function
x(h,f,n){var
d=b(i[23],h,f),l=d[3];if(l)var
m=a(g[r][1],l[1]);else
var
q=a(c[3],en),s=a(c[3],eo),t=a(c[13],0),u=a(aO[1],f),v=a(c[16],u),w=a(c[3],ep),x=j(K[6],e,k,n),y=a(c[3],eq),z=b(c[12],y,x),A=b(c[12],z,w),B=b(c[12],A,v),C=b(c[12],B,t),E=b(c[12],C,s),F=b(c[12],E,q),m=a(D(0),F);var
o=[0,d[1],d[2],0,d[4],d[5],d[6],d[7]],p=b(i[25],h,f);return[0,j(i[22],p,f,o),m]}function
y(c){var
b=a(f[30],c);if(3===b[0])return b[1][1];throw[0,ak,er]}function
m(j,i,g,b,a,f){var
c=b[2],d=b[1],k=a?a[1]:c,e=br(0,j,i,g,[0,d,k],f,0,h(d,c));return[0,e[1],[0,e[2],0]]}if(n){var
H=n[1],l=H[2],d=H[1];switch(l[0]){case
4:var
P=l[2],ah=l[3],ai=h(d,l[1]),u=h(d,ah),aj=y(P),Q=A(0,0,0,k,T,m(et,e,k,[0,d,u],0,E)),al=Q[2],am=Q[1],S=A(0,0,0,d,T,m(0,e,d,[0,d,P],0,E)),an=S[2],ao=S[1],U=A(0,w,0,k,q,m(0,e,k,[0,d,ai],0,E)),ap=U[2],aq=U[1],ar=s(am,e,o,1,function(b,j,q,f){var
k=a(g[9],u),l=a(g[9],j),c=R(b,a(i[V],d),l,k),e=x(c,aj,u),m=e[2],n=e[1];function
o(b,d,c,a){return s(aq,b,m,a,p)}return h(c,s(ao,b,h(n,u),f,o))}),as=a(ap,0)[3][2];a(an,0);a(al,0);return[0,ar,as];case
5:var
B=l[2],at=l[3],C=h(d,l[1]),v=h(d,at),au=y(B),av=a(g[9],C),W=R(e,d,a(g[9],B),av),X=A(0,0,0,k,T,m(eu,e,k,[0,W,h(W,v)],0,E)),aw=X[2],ax=X[1],Y=A(0,0,0,d,q,m(0,e,d,[0,d,B],0,E)),ay=Y[2],az=Y[1],aA=s(ax,e,o,1,function(b,k,q,j){var
l=a(g[9],v),m=a(g[9],k),c=R(b,a(i[V],d),m,l),e=x(c,au,v),f=e[1],n=e[2];function
o(b,j,i,d){var
e=a(g[9],C),c=h(R(b,f,a(g[9],n),e),C);return s(p,b,c,c,d)}return h(c,s(az,b,h(f,v),j,o))});a(ay,0);return[0,aA,a(aw,0)[3][2]];case
0:case
1:var
Z=h(d,l[1]),_=a(i[V],d);if(n)if(0===n[1][2][0])var
I=q,F=1;else
var
F=0;else
var
F=0;if(!F)var
I=T;var
J=A(0,w,0,k,I,m(0,e,k,[0,_,Z],0,E)),$=J[2],aa=s(J[1],e,o,1,p);return[0,aa,a($,0)[3][2]];default:var
L=l[1],t=h(d,l[2]);if(n)if(2===n[1][2][0])var
M=q,G=1;else
var
G=0;else
var
G=0;if(!G)var
M=T;var
ab=y(L),N=A(0,0,0,k,T,m(es,e,k,[0,d,t],0,E)),ac=N[2],ad=N[1],O=A(0,w,0,d,M,m(0,e,d,[0,d,L],0,E)),ae=O[2],af=O[1],ag=s(ad,e,o,1,function(c,k,r,j){var
l=a(g[9],t),m=a(g[9],k),e=R(c,a(i[V],d),m,l),f=x(e,ab,t),n=f[2],o=f[1];function
q(a,c){return b(p,a,n)}return h(e,s(af,c,h(o,t),j,q))});a(ae,0);return[0,ag,a(ac,0)[3][2]]}}var
aB=bn[2];return[0,s(p,e,o,o,1),aB]}function
bB(d,k,b){var
e=b[2],f=b[1],l=d?d[1]:0;switch(e[0]){case
1:case
3:var
o=a(c[3],ew),g=j(m[2],0,0,o);break;default:var
g=e[1]}var
h=l?aC(bm[22],0,0,0,ev,k,f):f,n=a(i[b2],h);return[0,z(h,g),n]}function
bC(o,h,n,m,g,d,l){if(a0(d,ex))var
j=0,i=ey;else
var
j=1,i=d;var
c=[0,0],k=bA(o,h,n,m,[0,g],i,function(m,d,k,g){aS(c,function(a){return d});if(j){var
h=b(e[4],g,l),i=b(e[5],h,1);return a(f[1],i)}return d}),p=k[2],q=k[1],r=0===a(e[3],c)?bB(0,h,g)[1]:aT(c);return[0,[0,r,p],q]}function
aV(g,f,e,d,c,b,a){return br(g,0,f,e,d,c,b,a)}function
ez(g,f,e,d,c,b,a){return bA(g,f,e,d,c,b,a)[1]}function
eA(e,p,o,d,n,c,m,l){var
q=c[2],t=c[1],u=a(g[r][1],n),v=a(g[r][1],p),w=a(i[V],t),h=aV(0,e,d,[0,w,a(g[r][1],q)],m,0,u),j=A(0,eB,0,d,o,[0,h[1],[0,h[2],0]]),x=j[2],y=j[1],z=s(y,e,v,l,function(c,b,a){return f[1]}),k=a(x,0),b=k[3],B=b[3],C=b[2],D=b[1],E=a(g[9],k[1]),F=a(g[9],z);return[0,D,C,a(g[9],B),F,E]}function
eC(g,n,r,d,l){var
e=l[2],h=l[1];try{var
f=eA(g,n,r,d,e,[0,h,e],E,1),p=f[1],F=f[4],G=f[3],H=f[2];if(p!==d)var
I=a(c[3],eF),q=j(m[5],0,0,I);else
var
q=[0,F,[0,b(i[a7],p,H),G]];return q}catch(f){f=t(f);if(f===x)try{var
z=function(a){return 1},k=av(0,g,d,a(i[V],h),e,z),o=k[1],A=k[3],B=k[2];if(o!==d)throw x;var
C=[0,n,[0,b(i[a7],o,B),A]];return C}catch(d){var
s=a(c[3],eD),u=aP(g,h,e),v=a(c[3],eE),w=b(c[12],v,u),y=b(c[12],w,s);return a(D(0),y)}throw f}}function
eG(b,e,d){var
f=a(l[2],b),g=a(l[5],b),c=eC(g,a(l[4],b),e,f,d);return[0,c[1],c[2][2]]}function
eH(a){var
c=[0,[0,n[1][11][1],0,an[3][2]]];return[0,32,[0,b(v[3],0,[0,[0,a],0]),0],c]}function
eI(d){var
b=d[2],c=b[2],e=a(v[1],b[1]),f=c?12===c[1][1][0]?1:0:13===e[0]?1:0;return f?1:0}function
eJ(d,c){var
e=a(l[2],c),f=b(i[a7],e,d),g=a(i[89],c);return b(l[3],g,f)}function
eK(d,c){var
e=a(l[2],c),f=b(i[a3],e,d),g=a(i[89],c);return b(l[3],g,f)}function
bD(a,b){return by([0,aU],a,b,0)}var
eO=[0,function(m,c){var
d=c[1],e=a(n[1][6],eN),f=b(n[1][11][23],e,d),h=a(B[6],aU),A=b(an[2][7],h,f);function
k(c){var
p=bD(c,A),q=a(l[2],c),s=a(l[4],c),t=a(g[r][1],s),e=bC(0,a(l[5],c),q,t,p,T,1),u=e[2],f=a(g[9],e[1][1]),v=a(g[9],u),d=b(l[12],c,f),h=d[2],k=d[1],m=a(i[89],c),o=b(l[3],m,k),w=[0,a(n[1][6],eL)],x=[0,b(S[4],w,0),f,h,v],y=a(g[22],x),z=j(eM[3],1,y,2);return b(am[71][7],z,o)}return b(am[71][1],0,k)}];j(bF[16],0,bE,eO);var
eP=[31,b(N[1],0,[0,[0,bE,0],0])],eR=[28,[0,[0,[0,a(n[1][6],eQ)],0],eP]];function
eS(c){var
b=a(n[1][6],eT);return $(bF[10],1,0,0,b,eR)}b(bG[13],eS,eU);function
bH(n,d){var
o=a(l[4],d),f=a(l[2],d),h=a(l[5],d),p=a(g[r][1],o),i=bz(d,n,0),k=i[2],q=i[1];if(0===k[0])var
e=k[1];else
var
y=a(c[3],e2),e=a(D(0),y);var
u=0,m=aV(0,h,f,[0,q,e],function(a,b){return 1},u,e),v=A(eW,eV,0,f,0,[0,m[1],[0,m[2],0]])[1];function
w(A,f,e,z){var
g=d[2],h=a(l[5],d),i=j(K[6],h,g,e),k=a(c[13],0),m=a(c[3],eX),n=a(c[13],0),o=d[2],p=a(l[5],d),q=j(K[6],p,o,f),r=a(c[13],0),s=a(c[3],eY),t=b(c[12],s,r),u=b(c[12],t,q),v=b(c[12],u,n),w=b(c[12],v,m),x=b(c[12],w,k),y=b(c[12],x,i);b(aE,0,b(c[26],1,y));return e}b(aE,0,a(c[3],eZ));try{for(;;){s(v,h,p,1,w);continue}}catch(e){e=t(e);if(e===x){b(aE,0,a(c[3],e0));return a(e1[1],d)}throw e}}var
k=[0,aU,d2,d4,d5,ay,function(a){return a},bj,bi,bx,G,d$,aG];aX(217,[0,aG,ay,x,X,dK,bB,bD,bz,ez,bC,Y,aV,A,eG,d7,aS,aT,R,cA,d6,P,em,eI,eH,F,aP,eJ,eK,a_,bH,k],"Ssrmatching_plugin__Ssrmatching");var
e3=a(y[6],0);a(bG[9],bI);function
Z(c,b,a){return k[5]}function
e4(b,a){return Z}function
e5(b,a){return Z}var
e6=[0,function(b,a){return Z},e5,e4],e7=[2,k[4]],e8=[0,k[3]],e9=k[2],e_=[0,function(a,c){return[0,a,b(e9,a,c)]}],e$=a(B[6],k[1]),fa=[0,a(W[3],e$)],fb=0;function
fc(c,e){var
d=[0,b(k[7],c,0)];return a(k[6],d)}var
fd=[0,[0,[0,0,[6,q[16][3]]],fc],fb];function
fe(c,f,e){var
d=[1,b(k[7],c,0)];return a(k[6],d)}var
ff=[6,q[16][3]],fh=[0,[0,[0,[0,0,[0,a(y[10],fg)]],ff],fe],fd];function
fi(d,h,c,g){var
e=b(k[7],d,0),f=[2,b(k[7],c,0),e];return a(k[6],f)}var
fj=[6,q[16][3]],fl=[0,a(y[10],fk)],fm=[0,[0,[0,[0,[0,0,[6,q[16][3]]],fl],fj],fi],fh];function
fn(d,i,c,h,g){var
e=b(k[7],d,0),f=[3,b(k[7],c,0),e];return a(k[6],f)}var
fo=[6,q[16][3]],fq=[0,a(y[10],fp)],fr=[6,q[16][3]],ft=[0,[0,[0,[0,[0,[0,0,[0,a(y[10],fs)]],fr],fq],fo],fn],fm];function
fu(e,l,d,j,c,i){var
f=b(k[7],e,0),g=b(k[7],d,0),h=[4,b(k[7],c,0),g,f];return a(k[6],h)}var
fv=[6,q[16][3]],fx=[0,a(y[10],fw)],fy=[6,q[16][3]],fA=[0,a(y[10],fz)],fB=[0,[0,[0,[0,[0,[0,[0,0,[6,q[16][3]]],fA],fy],fx],fv],fu],ft];function
fC(e,l,d,j,c,i){var
f=b(k[7],e,0),g=b(k[7],d,0),h=[5,b(k[7],c,0),g,f];return a(k[6],h)}var
fD=[6,q[16][3]],fF=[0,a(y[10],fE)],fG=[6,q[16][3]],fI=[0,a(y[10],fH)],bJ=b(ap[9],fJ,[0,[1,[0,[0,[0,[0,[0,[0,[0,0,[6,q[16][3]]],fI],fG],fF],fD],fC],fB]],fa,e_,e8,e7,e6]),bK=bJ[2],aq=bJ[1];function
_(c,b,a){return k[12]}function
fK(b,a){return _}function
fL(b,a){return _}var
fM=[0,function(b,a){return _},fL,fK],fN=[2,k[11]],fO=[0,k[10]],fP=k[9],fQ=[0,function(a,c){return[0,a,b(fP,a,c)]}],fR=0,fS=0;function
fT(a,d,c){return b(k[7],a,0)}var
fU=[6,q[16][1]],fW=[0,[1,[0,[0,[0,[0,0,[0,a(y[10],fV)]],fU],fT],fS]],fR,fQ,fO,fN,fM],bL=b(ap[9],fX,fW),bM=bL[2],aW=bL[1];function
fY(f,d){var
a=b(e[28],0,d);if(typeof
a!=="number"&&0===a[0]){var
c=a[1];if(!J(c,fZ))return 40;if(!J(c,f0))return 64}return 32}var
bN=b(q[2][4],f1,fY),f2=0,f3=0;function
f4(b,a,d){var
c=j(k[8],a,b,0);if(aZ(P(c),[0,d]))if(40===a)return j(k[8],a4,b,0);return c}j(q[19],bM,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,bN]],[6,q[16][1]]],f4],f3]],f2]]);function
f5(b,a){return _}function
f6(b,a){return _}var
f7=[0,function(b,a){return _},f6,f5],f8=[2,k[11]],f9=[0,k[10]],f_=k[9],f$=[0,function(a,c){return[0,a,b(f_,a,c)]}],ga=a(B[6],aW),gb=[0,a(W[3],ga)],gc=0;function
gd(a,d,c){return b(k[7],a,0)}var
ge=[6,q[16][3]],gg=[0,[1,[0,[0,[0,[0,0,[0,a(y[10],gf)]],ge],gd],gc]],gb,f$,f9,f8,f7],bO=b(ap[9],gh,gg),bP=bO[2],gi=bO[1],gj=0,gk=0;function
gl(b,a,d){var
c=j(k[8],a,b,0);if(aZ(P(c),[0,d]))if(40===a)return j(k[8],a4,b,0);return c}j(q[19],bP,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[6,bN]],[6,q[16][3]]],gl],gk]],gj]]);function
gm(b,a){return Z}function
gn(b,a){return Z}var
go=[0,function(b,a){return Z},gn,gm],gp=a(B[6],aq),gq=[0,[0,bK],[0,a(W[3],gp)],[1,aq],[1,aq],[1,aq],go];b(ap[9],gr,gq);var
gs=0;function
gt(a,d){function
c(b){return bH(a,b)}return b(am[71][1],0,c)}var
gv=[0,[0,[0,gu,[1,[5,a(B[16],aW)],0]],gt],gs];$(ap[8],bI,gw,0,0,gv);a(y[5],e3);aX(221,[0,bM,aW,bP,gi,bK,aq],"Ssrmatching_plugin__G_ssrmatching");return}

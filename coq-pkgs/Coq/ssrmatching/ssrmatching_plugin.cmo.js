(function(hf){"use strict";var
b$="Only identifiers are allowed here",co=123,q=140,bl=145,aO=112,cn="partial term ",cf="mk_tpattern_matcher with no upats_origin",aa="in",ce="do_once never called",cm="(",cd="ssrinstoftpat",$=173,bk="rpattern",al="In",cg="ssrpattern",cl="pattern",L=124,_=246,b_=111,bi="As",u=" in ",bp=120,b9="_ssrpat_",cc=113,ck="The ",aM="!! ",cb=122,ak="in ",bo="cpattern",aN=143,bn="lcpattern",cj="!! %-39s %10d %9.4f %9.4f %9.4f",b8="ssrpatternarg",ci="total",ab="plugins/ssrmatching/ssrmatching.ml4",aP="ssrmatching_plugin",G=136,bm=248,ch="Qed",ca=" of ",bh=146,bj=" as ",n=hf.jsoo_runtime,Q=n.caml_check_bound,bg=n.caml_equal,aL=n.caml_fresh_oo_id,b6=n.caml_ml_string_length,d=n.caml_new_string,bf=n.caml_notequal,b7=n.caml_register_global,b3=n.caml_string_get,R=n.caml_string_notequal,t=n.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):n.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):n.caml_call_gen(a,[b,c])}function
h(a,b,c,d){return a.length==3?a(b,c,d):n.caml_call_gen(a,[b,c,d])}function
m(a,b,c,d,e){return a.length==4?a(b,c,d,e):n.caml_call_gen(a,[b,c,d,e])}function
b4(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):n.caml_call_gen(a,[b,c,d,e,f])}function
aj(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):n.caml_call_gen(a,[b,c,d,e,f,g])}function
b5(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):n.caml_call_gen(a,[b,c,d,e,f,g,h])}var
f=n.caml_get_global_data(),ha=[0,4],hb=[0,1,9],hc=[0,1,9],hd=[0,4],he=[0,1,9],aY=d(aP),Z=[0,[0,0,0]],b1=[0,d(aP),d(cg)],S=f.Assert_failure,r=f.Names,aW=f.Loc,ao=f.Tacentries,ap=f.Proofview,k=f.Genarg,ac=f.Tacinterp,I=f.Pervasives,c=f.Pp,l=f.Tacmach,aS=f.Tacenv,aX=f.Global,e=f.Term,g=f.Evd,o=f.Geninterp,N=f.Ftactic,aT=f.Sigma,s=f.CErrors,bq=f.Typeclasses,H=f.Reductionops,bw=f.Evar,i=f.Util,M=f.Ppconstr,ad=f.Option,at=f.Context,am=f.Not_found,T=f.Evarutil,aQ=f.Vars,ar=f.Goal,as=f.Environ,bz=f.Compat,by=f.Constrexpr_ops,br=f.Termops,bs=f.Recordops,bv=f.Evarconv,aR=f.Unification,aB=f.Printf,an=f.Unix,C=f.Genintern,aq=f.Pptactic,aU=f.Printer,bx=f.Constrintern,bu=f.Feedback,j=f.Pcoq,aV=f.Mltop,bt=f.Goptions,x=f.CLexer,cs=f.Tacticals,cr=f.Tactics,cz=f.Constrarg,ct=f.Tacsubst,cu=f.Tacintern,cB=f.CArray,cp=f.Failure,cw=f.Pretype_errors,cq=f.Invalid_argument,cx=f.Globnames,cv=f.Reduction,cy=f.Glob_ops,cA=f.CamlinternalLazy,cC=a(x[8],0);a(aV[12],d(aP));var
y=aW[4],O=a(s[7],d("ssrmatching")),g8=[0,d(ab),1,0],g7=d("$arg"),g9=[0,d("ssrinstancesoftpat")],g_=d(cd),g2=d("Extension: cannot occur"),gW=d("matches:"),gX=d("instance:"),g0=d("Not supported"),gU=[0,1],gV=[0,1],gY=d("BEGIN INSTANCES"),gZ=d("END INSTANCES"),gR=d(cg),gL=d(cl),gK=d("selected"),gw=d("matching impacts evars"),gu=d(" does not match any subterm of the goal"),gv=d(cn),gs=[0,1],gp=[0,[0,1,0]],gq=[0,[0,0,[0,1,0]]],go=d("pattern without redex"),gn=[0,0],gj=[0,d(ab),1229,56],gf=d("in the pattern?"),gg=d('Does the variable bound by the "in" construct occur '),gh=d(" did not instantiate ?"),gi=d("Matching the pattern "),gl=[0,1],gm=[0,1],gk=[0,1],gd=d("typed as: "),gc=d("decoded as: "),gb=[0,d(ab),1155,54],f_=d(b9),f$=d(bi),ga=d(al),f9=[0,d(ab),1115,63],f8=d("bad encoding for pattern "),f7=d(" in ist: "),f6=d("interpreting: "),f5=d(":"),fr=d(cm),fs=d("@"),fp=[0,d(ab),1013,12],fo=d(b$),er=d(b$),eq=d(b9),ep=d("globbing pattern: "),es=d("( _ as _ )"),et=d("( _ as _ in _ )"),eu=d("( _ in _ )"),ev=d("( _ in _ in _ )"),ew=d(al),ey=d(al),ez=d(al),eB=d(al),eA=d("where are we?"),ex=d(al),eC=d(bi),eD=d(bi),ef=d(ak),eg=d(u),eh=d(u),ei=d(ak),ej=d(u),ek=d(u),el=d(u),em=d(bj),d9=d(ak),d_=d(u),d$=d(u),ea=d(ak),eb=d(u),ec=d(u),ed=d(u),ee=d(bj),d1=d(ak),d2=d(u),d3=d(u),d4=d(ak),d5=d(u),d6=d(u),d7=d(u),d8=d(bj),dZ=d("matches but type classes inference fails"),d0=d("does not match any subterm of the goal"),dY=d(cf),dW=d("are equal to the "),dX=d("all matches of "),dV=d("companion function never called"),dP=d("of "),dQ=d(" of the "),dU=d(" of"),dR=d(" occurence"),dS=d(" < "),dT=d("Only "),dK=d(ca),dL=d(ck),dI=d(ca),dJ=d(ck),dN=d("term "),dO=d(cn),dM=d(cf),dH=d(ce),dF=d(ce),dx=d("incomplete ise in match_upats_FO"),dy=d("IN FO"),dr=d(u),ds=d("indeterminate "),dt=d("indeterminate pattern"),dk=d("RHS"),dj=d("LHS"),dg=[0,1],db=[0,[11,d(aM),[2,[0,0,39],[12,32,[4,0,[0,1,10],0,[12,32,[8,0,[0,1,9],[0,4],[12,32,[8,0,[0,1,9],[0,4],[12,32,[8,0,hb,ha,0]]]]]]]]]],d(cj)],da=[0,d(ab),214,26],c4=[0,[11,d(aM),[2,[0,1,39],[11,d(" ---------- --------- --------- ---------"),0]]],d("!! %39s ---------- --------- --------- ---------")],c5=d("average"),c6=d("max"),c7=d(ci),c8=d("#calls"),c9=d("function"),c_=[0,[11,d(aM),[2,[0,0,39],[12,32,[2,[0,1,10],[12,32,[2,[0,1,9],[12,32,[2,[0,1,9],[12,32,[2,hc,0]]]]]]]]]],d("!! %-39s %10s %9s %9s %9s")],c1=[0,d(ab),207,26],cY=d(ci),cZ=[0,[11,d(aM),[2,[0,0,39],[12,32,[4,0,[0,1,10],0,[12,32,[8,0,[0,1,9],[0,4],[12,32,[8,0,[0,1,9],[0,4],[12,32,[8,0,he,hd,0]]]]]]]]]],d(cj)],cR=d("have: mixed C-G constr"),cS=d("have: mixed G-C constr"),cQ=[0,0],cP=d("not a CRef"),cJ=d("$"),cH=d(")"),cI=d(cm),cD=d("SSR: "),g$=d("SSRMATCHINGDEBUG"),cE=[0,d("Debug"),[0,d("SsrMatching"),0]],cF=d("ssrmatching debugging"),cU=[0,d("SsrMatchingProfiling"),0],cV=d("ssrmatching profiling"),dc=d("Ssrmatching.NoProgress"),dd=d("unif_EQ_args"),di=d("Ssrmatching.NoMatch"),dn=d("_"),dv=d("Ssrmatching.FoundUnif"),dz=d("match_upats_FO"),dC=d("match_upats_HO"),eo=d("rpatternty"),eF=d(bk),eL=d(bk),eS=d(aa),eW=d(aa),e1=d(aa),e4=d(aa),e8=d(aa),e$=d(aa),fe=d(aa),fh=d("as"),fl=d(bk),ft=d("ssrtermkind"),fu=d(bo),fy=d(bo),fD=d(ch),fH=d(bo),fL=d(bn),fR=d(bn),fW=d(ch),f0=d(bn),gB=d(b8),gH=d(b8),gO=d(cl),gS=d(aP),g5=d(cd);function
bA(d,b){var
e=[0,d,b,a(c[1],b)];return a(s[8],e)}var
aZ=bu[12],au=[0,function(a){return 0}];function
a0(d){var
e=n.caml_obj_tag(d),f=250===e?d[1]:_===e?a(cA[2],d):d,g=a(c[1],cD),h=b(c[13],g,f);return b(bu[16],0,h)}try{n.caml_sys_getenv(g$);au[1]=a0}catch(a){a=t(a);if(a!==am)throw a}function
bB(a){return a?(au[1]=a0,0):(au[1]=function(a){return 0},0)}var
cG=[0,0,0,cF,cE,function(a){return au[1]===a0?1:0},bB];b(bt[4],0,cG);function
ae(b){return a(au[1],b)}function
bC(c){var
b=a(e[q],c);return 9===b[0]?[0,b[1],b[2]]:[0,c,[0]]}function
bD(e,d,c){var
a=b3(d,c);if(48<=a)var
b=61===a?1:co===a?1:0;else{if(40===a)return 0;var
b=47<=a?1:0}return b?1:40===e?1:0}function
bE(m,f,e){var
n=a(f,e),o=a(c[42],n),g=b(I[16],o,cJ),d=0;for(;;){if(22<(b3(g,d)-10|0)>>>0){if(b(m,g,d)){var
h=a(c[1],cH),i=a(f,e),j=a(c[1],cI),k=b(c[13],j,i),l=b(c[13],k,h);return b(c[29],1,l)}return a(f,e)}var
d=d+1|0;continue}}var
aC=aU[5],cK=M[24],cL=M[23];function
cM(c){var
d=c[2],f=c[1];if(d)return a(cK,d[1]);var
e=a(aX[2],0);return b(aU[28],e,f)}function
cN(c){var
d=c[2],f=c[1];if(d)return a(cL,d[1]);var
e=a(aX[2],0);return b(aU[30],e,f)}function
a1(a){var
b=a[2],c=a[1];return bE(function(a,b){return bD(c,a,b)},cN,b)}function
p(a){var
b=a[2],c=a[1];return bE(function(a,b){return bD(c,a,b)},cM,b)}function
cO(e,g){var
c=a(k[2],e),f=a(o[1][1],e);function
h(b,a){return[0,b,a]}function
i(b,a){return a}function
j(c,b){return a(N[1],[0,f,b])}function
d(c,b,a){return g}b(C[5],c,h);b(C[6],c,i);b(o[6],c,j);b(o[3],c,[0,[0,f]]);m(aq[1],c,d,d,d);return c}function
a2(a){if(0===a[0])if(0!==a[1][0])return 1;return 0}function
a3(a){return[12,a,0,0,0]}function
aD(c,b,a){return[16,c,b,[0,a]]}var
U=[13,[0,y,0,0,0]];function
av(b,a){return[14,y,b,[0,a]]}function
a4(e,d,m,l){var
f=e[2],g=f[2],i=e[1],n=f[1];if(g){var
j=d[2][2];if(j)return[0,i,[0,U,[0,b(m,g[1],j[1])]]];var
o=a(c[1],cR);return h(s[3],0,0,o)}var
k=d[2];if(k[2]){var
p=a(c[1],cS);return h(s[3],0,0,p)}return[0,i,[0,b(l,n,k[1]),0]]}function
V(d){var
b=d[2],c=b[2],e=b[1];return c?a(by[6],c[1]):a(cy[15],e)}function
aw(b,a){return[0,b,[0,U,[0,a]]]}var
cT=32;function
v(a){return aw(cT,a)}var
a5=[0,0],a6=[0,0],aE=[0,0];function
a7(a){aE[1]=[0,a,aE[1]];return 0}function
bF(c){a5[1]=c;if(c){var
e=aE[1],f=function(b){return a(b[2],0)};b(i[17][11],f,e)}var
d=1-c;if(d){var
g=aE[1],h=function(b){return a(b[3],0)};return b(i[17][11],h,g)}return d}var
cW=[0,0,0,cV,cU,function(a){return a5[1]},bF];b(bt[4],0,cW);var
bG=[0,0];function
cX(f){var
b=a6[1];if(b){var
c=bG[1],d=a(an[87],0)-c,e=aj(aB[4],cZ,cY,0,d,0,0);return a(I[38],e)}return b}function
c0(b){bG[1]=a(an[87],0);return 0}var
c2=[0,function(b,a){throw[0,S,c1]},c0,cX];function
c3(g){var
c=a6[1];if(c){var
d=b(i[15][1],39,45),e=b(aB[4],c4,d);a(I[38],e);var
f=aj(aB[4],c_,c9,c8,c7,c6,c5);return a(I[38],f)}return c}function
c$(a){return 0}a7([0,function(b,a){throw[0,S,da]},c$,c3]);a7(c2);function
a8(f){var
c=[0,0],d=[0,0],b=[0,0];function
g(a){b[1]=0;d[1]=0;c[1]=0;return 0}function
h(h,g){if(a5[1]){var
i=a(an[87],0);try{d[1]++;var
j=a(h,g),f=a(an[87],0)-i;b[1]=b[1]+f;if(c[1]<f)c[1]=f;return j}catch(d){d=t(d);var
e=a(an[87],0)-i;b[1]=b[1]+e;if(c[1]<e)c[1]=e;throw d}}return a(h,g)}var
e=[0,h,g,function(h){var
e=0!==d[1]?1:0;if(e){a6[1]=1;var
g=aj(aB[4],db,f,d[1],b[1],c[1],b[1]/d[1]);return a(I[38],g)}return e}];a7(e);return e}var
af=[bm,dc,aL(0)];function
ax(e,b,d,c){var
f=a(g[144],b),h=[0,a(g[42],b),f];try{aj(cv[12],0,0,e,[0,h],d,c);var
i=1;return i}catch(a){return 0}}var
de=a8(dd);function
W(d,c,b,a){return b4(bv[2],d,0,b,a,c)}function
bH(h,g,c,f,e){var
b=g,a=0,i=c.length-1;for(;;){if(a===i)return b;var
d=f+a|0,j=Q(e,d)[d+1],b=W(h,b,Q(c,a)[a+1],j),a=a+1|0;continue}}var
X=a(aR[4],0)[1].slice();X[1]=0;X[5]=r[61];X[11]=1;X[12]=1;var
df=[0,X,X,X,0,a(aR[4],0)[5]];function
a9(d,c,b,a){return aj(aR[8],d,c,0,[0,df],b,a)}function
a_(j,d,k){var
c=[0,j];function
f(l){var
m=a(e[q],l);if(3===m[0]){var
k=m[1];try{var
r=f(b(g[40],d,k));return r}catch(l){var
j=k[1],n=b(i[19][15],f,k[2]);if(1-b(g[26],c[1],j)){var
o=b(g[23],d,j),p=b(T[40],d,o);c[1]=h(g[22],c[1],j,p)}return a(e[115],[0,j,n])}}return b(e[aN],f,l)}function
l(e,j,n){if(0===a(g[10],j)){var
k=b(g[23],d,e),i=a(g[10],k);if(i){var
l=c[1],m=f(i[1]);c[1]=h(g[31],e,m,l);return 0}return 0}return 0}var
m=f(k);h(g[27],l,j,0);var
n=a(g[q],d);return[0,c[1],n,m]}function
aF(i,f,q,p,o){var
j=h(bv[6],i,0,q),c=a_(f,j,p),k=c[3],d=c[2],l=c[1],r=a(g[$],l),m=b(g[bh],r,d),n=b5(bq[29],0,0,0,0,dg,i,m);if(a(o,j)){if(n===m)return[0,l,d,k];var
e=a_(f,n,k),s=e[3],t=e[1];return[0,t,b(g[aO],d,e[2]),s]}throw af}function
Y(d,c,f,a){var
h=W(d,c,f,a),e=aF(d,c,h,a,function(a){return 1});return b(g[bh],e[1],e[2])}function
dh(c,e,d){var
f=a(g[68],c),h=a(l[2],c),i=Y(a(l[8],c),h,e,d);return b(l[3],f,i)}function
bI(c,k){var
d=a(e[q],k);switch(d[0]){case
3:return b(i[19][13],c,d[1][2]);case
5:var
l=d[3];a(c,d[1]);return a(c,l);case
8:var
n=d[4],o=d[3];a(c,d[2]);a(c,o);return a(c,n);case
9:var
p=d[2];a(c,d[1]);return b(i[19][13],c,p);case
13:var
r=d[4],s=d[2];a(c,d[3]);a(c,s);return b(i[19][13],c,r);case
6:case
7:var
m=d[3];a(c,d[2]);return a(c,m);case
14:case
15:var
g=d[1][2],h=g[2],j=h.length-1-1|0,t=g[3],u=0;if(!(j<0)){var
f=u;for(;;){a(c,Q(h,f)[f+1]);a(c,Q(t,f)[f+1]);var
v=f+1|0;if(j!==f){var
f=v;continue}break}}return 0;default:return 0}}var
z=[bm,di,aL(0)];function
ag(b){return 0===b?a(c[1],dj):a(c[1],dk)}function
dl(a){return 0===a?1:0}function
J(b,a){return 1}function
dm(b){try{var
c=1+a(bs[4],[1,b])|0;return c}catch(a){return 0}}function
bJ(b){switch(a(e[q],b)[0]){case
4:case
6:case
7:case
13:case
14:case
15:return 1;default:return 0}}aL(0);var
dp=a(r[69],dn),dq=a(e[cc],dp);function
A(d){function
c(d){return a(e[6],d)?dq:b(e[aN],c,d)}return a(aC,c(d))}function
bK(E,D,Y,C,B,X,W,V){var
v=B[1],Z=B[2],_=D?D[1]:0,F=b(H[30],v,V),d=F[2],f=F[1],o=a(e[q],f);switch(o[0]){case
3:var
J=o[1][1];if(b(g[26],C,J))var
r=[0,[0,J],f,d];else
if(0===d)if(E)var
K=E[1],aa=K[1],ab=A(K[2]),ac=a(c[1],dr),ad=ag(aa),ae=a(c[1],ds),af=b(c[13],ae,ad),ah=b(c[13],af,ac),r=a(O,b(c[13],ah,ab));else
var
r=a(s[6],dt);else
var
r=[0,5,f,d];var
k=r,l=0;break;case
7:var
k=[0,3,f,d],l=0;break;case
8:var
ai=o[4],aj=o[2],ak=bf(ai,a(e[aO],1))?[0,2,f,d]:[0,5,aj,d],k=ak,l=0;break;case
10:var
M=o[1][1],y=dm(M);if(0===y)var
z=0;else
if(a(i[17][1],d)<y)var
z=0;else
var
P=b(i[17][99],y,d),al=P[2],N=[0,[1,M],a(e[59],[0,f,P[1]]),al],z=1;if(!z)var
N=[0,1,f,d];var
k=N,l=0;break;case
1:case
11:case
12:var
x=0,p=f,w=d,l=1;break;default:var
x=4,p=f,w=d,l=1}if(!l)var
x=k[1],p=k[2],w=k[3];var
G=a(i[19][12],w),u=[0,v],n=[0,v],$=a(e[L],[0,p,G]),R=_?1:0,Q=a(as[9],Y),S=a(i[17][1],Q)+R|0;function
j(r){var
c=r;for(;;){var
f=a(e[q],c);if(3===f[0]){var
d=f[1],k=d[1],s=d[2];try{var
G=j(b(g[40],n[1],d));return G}catch(f){f=t(f);if(f===g[39]){if(b(g[26],C,k))return b(e[aN],j,c);var
l=b(g[23],n[1],k),v=a(g[7],l),w=b(I[5],0,s.length-1-S|0),x=b(i[17][104],w,v),y=function(c,b){var
d=c[2],f=c[1];if(0===b[0]){var
g=b[1],i=j(b[2]),k=h(e[52],g,i,d);return[0,[0,a(e[cc],g),f],k]}var
l=b[2],n=b[1],o=j(b[3]),p=j(l);return[0,f,m(e[51],n,p,o,d)]},z=[0,0,j(l[1])],o=h(at[2][9],y,z,x),A=o[2],B=o[1],p=a(T[1],0);u[1]=m(g[95],p,A,0,u[1]);var
D=n[1],E=[0,a(e[114],p),B],F=a(e[59],E);n[1]=h(g[31],k,F,D);var
c=b(g[40],n[1],d);continue}throw f}}return b(e[aN],j,c)}}var
U=j($);return[0,u[1],[0,x,U,p,G,Z,W,X]]}function
bL(h,d,f){var
i=d[1],n=d[3],o=d[2],j=bC(h),k=j[1],p=j[2],l=a(e[q],k);switch(l[0]){case
3:var
m=l[1][1];if(b(g[34],i,m))throw z;var
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
c=4}return[0,i,o,[0,c,h,k,p,n,f[6],f[7]]]}function
du(j,h,k){function
f(b){var
c=a(bs[8],[0,[1,j],b])[2][7];return a(i[17][1],c)}try{var
g=a(e[q],h);switch(g[0]){case
4:var
d=f([1,a(e[b_],g[1])]),c=1;break;case
6:var
d=f(0),c=1;break;case
10:if(b(r[17][13],g[1][1],j))var
d=a(e[37],k[3])[2].length-1,c=1;else
var
c=0;break;case
1:case
11:case
12:var
c=0;break;default:var
d=-1,c=1}if(!c)var
d=f([0,a(cx[16],h)]);return d}catch(a){a=t(a);if(a===am)return-1;throw a}}function
bM(d,c){var
b=a(e[q],c);return 3===b[0]?bg(d,b[1][1]):0}function
aG(d,c,b){if(0===c)return d;var
f=c===b.length-1?b:h(i[19][7],b,0,c);return a(e[L],[0,d,f])}function
a$(j){function
h(l,k){var
d=l,c=k;for(;;){var
f=a(e[q],d);switch(f[0]){case
3:var
m=f[1];try{var
n=h(b(g[40],j,m),c);return n}catch(a){return[0,d,c]}case
5:var
d=f[1];continue;case
9:var
o=f[1],d=o,c=b(i[19][5],f[2],c);continue;default:return[0,d,c]}}}return function(b){var
c=a(e[q],b);switch(c[0]){case
9:return h(c[1],c[2]);case
3:case
5:return h(b,[0]);default:return[0,b,[0]]}}}var
ay=[bm,dv,aL(0)];function
bN(c){var
d=a(g[84],c);return function(a){function
c(c){try{b(g[24],a,c);var
d=1;return d}catch(a){a=t(a);if(a===am)return 0;throw a}}return b(bw[6][15],c,d)}}function
dw(p,k,y,j,d){var
A=bN(d);function
f(r){var
l=a(a$(j),r),m=l[2],d=l[1],g=[0,-1],n=m.length-1,u=0;function
v(h,j){var
k=a(e[q],h[2]),i=9===k[0]?k[2].length-1:0;if(n<i)return j;var
f=h[1];if(typeof
f==="number")switch(f){case
0:var
c=b(e[G],h[3],d);break;case
1:var
c=b(e[G],h[3],d);break;case
2:var
c=a(e[14],d);break;case
3:var
c=a(e[13],d);break;case
4:var
c=bJ(d);break;default:g[1]=n;var
c=1}else
if(0===f[0])var
c=bM(f[1],d);else
var
l=a(e[125],f[1]),c=b(e[G],d,l);if(c){if(g[1]<i)g[1]=i;return[0,[0,h,i],j]}return j}var
w=h(i[17][16],v,p,u);for(;;){if(0<=g[1]){var
o=g[1];g[1]=-1;var
x=aG(d,o,m),B=function(o,l){return function(u){var
n=u[2],f=u[1];if(o<=n)var
q=o<n?1:0;else
if(5===f[1]){g[1]=o-1|0;var
q=0}else{if(g[1]<n)g[1]=n;var
q=1}if(!q)if(a(aQ[2],l))try{var
r=f[1];if(typeof
r==="number")if(2===r){var
v=function(f){var
d=bC(f),g=d[2],c=a(e[36],d[1]),h=c[4],j=c[3],k=c[1],l=b(i[19][39],c[2],g),m=[0,a(e[cb],[0,k,j,h]),l];return a(e[L],m)},C=v(l);a9(k,j,v(f[2]),C);var
p=1}else
if(5<=r){var
x=function(b){return a(e[cb],[0,0,e[117],b])},G=x(l);a9(k,j,x(f[2]),G);var
p=1}else
var
p=0;else
var
p=0;if(!p)a9(k,j,f[2],l);var
D=a(e[L],[0,f[3],f[4]]);try{var
E=W(k,j,D,l)}catch(a){throw z}var
w=aG(d,o,m),F=a(f[7],w);throw[0,ay,bL(w,aF(k,y,E,f[5],F),f)]}catch(b){b=t(b);if(b[1]===ay)if(a(A,b[2][1]))throw b;if(b===am){var
B=a(c[1],dx);return h(s[3],0,0,B)}return 0}return 0}}(o,x);b(i[17][11],B,w);continue}bI(f,d);return b(i[19][13],f,m)}}try{var
l=f(d);return l}catch(b){b=t(b);if(b[1]===cq){var
g=a(c[1],dy);return h(s[3],0,0,g)}throw b}}var
dA=a8(dz);function
dB(e,d,c,b,a){function
f(a,b){return dw(e,d,c,a,b)}return h(dA[1],f,b,a)}var
dD=a8(dC);function
dE(K,j,g,f,d,c){function
k(k,d){var
r=[0,0],u=[0,0],y=bN(d);function
c(l,f,o,g,p){var
n=a(a$(g),p),k=n[2],d=n[1],j=[0,-1],m=k.length-1,q=0;function
v(h,k){var
g=h[4].length-1;if(m<g)return k;var
i=h[1];if(typeof
i==="number")switch(i){case
0:if(b(e[G],h[3],d))var
f=g,c=1;else
var
c=0;break;case
1:if(b(e[G],h[3],d))var
f=g,c=1;else
var
c=0;break;case
2:if(a(e[14],d))var
f=g,c=1;else
var
c=0;break;case
3:if(a(e[13],d))var
f=g,c=1;else
var
c=0;break;case
4:if(bJ(d))var
f=g,c=1;else
var
c=0;break;default:var
f=g,c=1}else
if(0===i[0])if(bM(i[1],d))var
f=g,c=1;else
var
c=0;else
var
l=g+du(i[1],d,h)|0,n=m<l?-1:l,f=n,c=1;if(!c)var
f=-1;if(f<g)return k;if(j[1]<f)j[1]=m;return[0,[0,h,f],k]}var
w=h(i[17][16],v,l,q);for(;;){if(0<=j[1]){var
x=j[1];j[1]=-1;var
z=function(h){return function(v){var
m=v[2],c=v[1];if(h<=m)var
p=h<m?1:0;else
if(5===c[1]){j[1]=h-1|0;var
p=0}else{if(j[1]<m)j[1]=m;var
p=1}if(p)return 0;try{var
q=c[1];if(typeof
q==="number")switch(q){case
2:var
n=a(e[36],d),C=n[4],D=n[3],E=n[2],F=n[1],x=a(e[36],c[3]),G=x[4],H=W(f,g,x[2],E),l=W(b(as[20],[0,F,D],f),H,G,C),i=1;break;case
5:var
i=0;break;case
3:case
4:var
l=W(f,g,c[3],d),i=1;break;default:var
l=g,i=1}else
if(0===q[0])var
J=a(e[42],c[3])[2],l=bH(f,g,J,0,a(e[42],d)[2]),i=1;else
var
i=0;if(!i)var
I=aG(d,h-(c[4].length-1)|0,k),l=W(f,g,c[3],I);var
z=bH(f,l,c[4],h-(c[4].length-1)|0,k),w=aG(d,h,k),A=a(c[7],w),B=a(K,bL(w,aF(f,o,z,c[5],A),c));return B}catch(b){b=t(b);if(b[1]===ay)if(a(y,b[2][1]))throw b;if(b===af){r[1]=1;return 0}if(b[1]===cw[1])if(18===b[4][0]){u[1]=1;return 0}if(a(s[22],b))return 0;throw b}}}(x);b(i[17][11],z,w);continue}bI(function(a){return c(l,f,o,g,a)},d);var
A=function(a){return c(l,f,o,g,a)};return b(i[19][13],A,k)}}c(j,g,f,k,d);if(r[1])throw af;return u[1]}return h(dD[1],k,d,c)}function
ba(b,c){return b[1]?0:(b[1]=[0,a(c,0)],0)}function
bb(d){var
b=d[1];if(b)return b[1];var
e=a(c[1],dF);return h(s[3],0,0,e)}function
dG(d){var
e=d[1];if(e){var
f=e[1],g=f[2],j=f[1];d[1]=[0,[0,j+1|0,g]];try{var
k=b(i[17][5],g,j);return k}catch(a){a=t(a);if(a[1]===cp)throw z;throw a}}var
l=a(c[1],dH);return h(s[3],0,0,l)}function
B(B,y,l,F,x,r){var
d=r[2],J=r[1],u=B?B[1]:0,C=y?y[1]:0,g=[0,0],v=[0,0];if(x){var
k=x[1];if(0===k[1])var
E=k[2],j=0!==E?1:0,f=E;else
var
R=k[2],j=0===R?1:0,f=R}else
var
j=0,f=0;var
o=h(i[17][16],I[5],f,0),K=n.caml_make_vect(o,1-j);function
S(b){var
a=b-1|0;return Q(K,a)[a+1]=j}b(i[17][11],S,f);if(0===o)v[1]=j;var
w=[0,0];function
M(b){return a(e[L],[0,b[3],b[4]])}function
D(S){if(l){var
i=l[1],j=i[2],k=i[1];if(d)if(!d[2]){var
w=d[1],x=a(c[6],0),y=A(M(w)),z=a(c[8],4),B=a(c[6],0),C=A(j),D=a(c[1],dK),E=ag(k),F=a(c[1],dL),G=b(c[13],F,E),H=b(c[13],G,D),I=b(c[13],H,C),J=b(c[13],I,B),K=b(c[13],J,z),L=b(c[13],K,y);return b(c[13],L,x)}var
n=a(c[16],0),o=A(j),p=a(c[1],dI),q=ag(k),r=a(c[1],dJ),t=b(c[13],r,q),u=b(c[13],t,p),v=b(c[13],u,o);return b(c[13],v,n)}if(d)if(!d[2]){var
e=d[1],O=a(c[16],0),P=A(M(e)),g=e[1];if(typeof
g==="number")if(5<=g)var
f=0;else
var
m=1-a(br[38],e[5]),f=1;else
var
f=0;if(!f)var
m=0;var
Q=m?a(c[1],dN):a(c[1],dO),R=b(c[13],Q,P);return b(c[13],R,O)}var
N=a(c[1],dM);return h(s[3],0,0,N)}var
p=[0,0];function
N(a){return p[1]}function
T(a){if(u){p[1]=b(i[18],p[1],[0,a,0]);return 0}throw[0,ay,a]}function
P(c){if(c){var
d=c[1],f=d[3],g=d[1],j=c[2],k=f[4],l=f[3],m=b(H[19],g,f[5]),n=b(H[19],g,l),o=a(H[19],g),p=b(i[19][15],o,k),q=function(f){var
c=f[3],d=f[1],l=c[4],o=c[3],q=b(H[19],d,c[5]),r=b(H[19],d,o),s=a(H[19],d),t=b(i[19][15],s,l),g=b(e[G],m,q);if(g)var
j=b(e[G],n,r),k=j?h(cB[31],e[G],p,t):j;else
var
k=g;return 1-k};return[0,d,P(b(i[17][29],q,j))]}return 0}function
U(aa){var
k=w[1];if(k)var
d=a(i[17][3],k[1][2]);else{if(C)throw z;var
$=a(c[1],dV),d=h(s[3],0,0,$)}var
f=d[3],p=d[2],q=d[1],j=a(e[L],[0,f[3],f[4]]);if(o<=g[1])return[0,j,f[6],[0,q,p,f[5]]];if(l)var
m=l[1],r=m[1],t=A(m[2]),u=a(c[1],dP),v=a(c[6],0),x=A(j),y=a(c[8],4),B=a(c[6],0),D=ag(r),E=a(c[1],dQ),F=b(c[13],E,D),G=b(c[13],F,B),H=b(c[13],G,y),I=b(c[13],H,x),J=b(c[13],I,v),K=b(c[13],J,u),n=b(c[13],K,t);else
var
X=A(j),Y=a(c[16],0),Z=a(c[1],dU),_=b(c[13],Z,Y),n=b(c[13],_,X);var
M=b(i[15][38],g[1],dR),N=a(c[1],M),P=a(c[19],o),Q=a(c[1],dS),R=a(c[19],g[1]),S=a(c[1],dT),T=b(c[13],S,R),U=b(c[13],T,Q),V=b(c[13],U,P),W=b(c[13],V,N);return a(O,b(c[13],W,n))}return[0,function(k,y,V,U){ba(w,function(A){var
e=[0,0];try{if(1-u)dB(d,k,F,J,y);e[1]=dE(T,d,k,F,J,y);throw z}catch(d){d=t(d);if(d[1]===ay)return[0,0,[0,d[2],0]];var
x=d===z?0:d===af?0:1;if(!x)if(u)if(0!==N(0))return[0,0,P(N(0))];if(d===z)if(!C){if(e[1]){var
q=a(c[25],dZ),r=D(0);return a(O,b(c[13],r,q))}var
v=a(c[1],d0),w=D(0);return a(O,b(c[13],w,v))}if(d===af){if(C)throw z;if(l)var
f=l[1][1];else
var
p=a(c[1],dY),f=h(s[3],0,0,p);var
g=ag(dl(f)),i=a(c[1],dW),j=D(0),m=a(c[1],dX),n=b(c[13],m,j),o=b(c[13],n,i);return a(O,b(c[13],o,g))}throw d}});if(u)var
E=dG(w);else
var
X=bb(w)[2],E=a(i[17][3],X);var
f=E[3],A=f[4],n=E[1];if(v[1])return y;var
H=f[1];if(typeof
H==="number")switch(H){case
0:var
r=a(e[G],f[3]),p=1;break;case
1:var
r=a(e[G],f[3]),p=1;break;case
2:var
x=a(e[36],f[3]),I=x[4],M=x[2],R=b(as[20],[0,x[1],x[3]],k),r=function(d){var
b=a(e[q],d);if(8===b[0]){var
f=b[4],c=ax(k,n,M,b[2]);return c?ax(R,n,I,f):c}return 0},p=1;break;case
3:var
r=function(b){return 7===a(e[q],b)[0]?ax(k,n,f[3],b):0},p=1;break;default:var
p=0}else
var
p=0;if(!p)var
S=f[3],r=function(a){return ax(k,n,S,a)};var
W=A.length-1;function
B(c,l){var
p=c[1],x=c[2];if(v[1])return l;var
q=a(a$(n),l),d=q[2],h=q[1];if(W<=d.length-1)if(a(r,h)){var
w=function(g){var
a=0,e=A.length-1;for(;;){var
b=a===e?1:0;if(b)var
c=b;else{var
f=Q(g,a)[a+1],d=ax(p,n,Q(A,a)[a+1],f);if(d){var
a=a+1|0;continue}var
c=d}return c}};if(b(de[1],w,d)){var
s=b(i[19][50],A.length-1,d),y=s[2],t=a(e[L],[0,h,s[1]]);g[1]++;if(g[1]===o)v[1]=j;if(g[1]<=o)var
k=g[1]-1|0,u=Q(K,k)[k+1];else
var
u=1-j;var
z=u?m(U,p,f[5],t,x):t,C=function(a){return B(c,a)},D=[0,z,b(i[19][56],C,y)];return a(e[L],D)}}function
E(a,c){var
d=c[2],e=c[1],f=0===a[0]?a:[0,a[1],a[3]];return[0,b(as[20],f,e),d+1|0]}var
F=m(br[29],E,B,c,h);function
G(a){return B(c,a)}var
H=[0,F,b(i[19][56],G,d)];return a(e[L],H)}return B([0,k,V],y)},U]}function
ah(d){switch(d[0]){case
0:return p(d[1]);case
1:var
e=p(d[1]),f=a(c[1],d1);return b(c[13],f,e);case
2:var
g=d[1],h=p(d[2]),i=a(c[1],d2),j=p(g),k=b(c[13],j,i);return b(c[13],k,h);case
3:var
l=d[1],m=p(d[2]),n=a(c[1],d3),o=p(l),q=a(c[1],d4),r=b(c[13],q,o),s=b(c[13],r,n);return b(c[13],s,m);case
4:var
t=d[2],u=d[1],v=p(d[3]),w=a(c[1],d5),x=p(t),y=a(c[1],d6),z=p(u),A=b(c[13],z,y),B=b(c[13],A,x),C=b(c[13],B,w);return b(c[13],C,v);default:var
D=d[2],E=d[1],F=p(d[3]),G=a(c[1],d7),H=p(D),I=a(c[1],d8),J=p(E),K=b(c[13],J,I),L=b(c[13],K,H),M=b(c[13],L,G);return b(c[13],M,F)}}function
bO(d){switch(d[0]){case
0:return p(d[1]);case
1:var
e=p(d[1]),f=a(c[1],d9);return b(c[13],f,e);case
2:var
g=d[1],h=p(d[2]),i=a(c[1],d_),j=a(M[12],g),k=b(c[13],j,i);return b(c[13],k,h);case
3:var
l=d[1],m=p(d[2]),n=a(c[1],d$),o=a(M[12],l),q=a(c[1],ea),r=b(c[13],q,o),s=b(c[13],r,n);return b(c[13],s,m);case
4:var
t=d[2],u=d[1],v=p(d[3]),w=a(c[1],eb),x=a(M[12],t),y=a(c[1],ec),z=p(u),A=b(c[13],z,y),B=b(c[13],A,x),C=b(c[13],B,w);return b(c[13],C,v);default:var
D=d[2],E=d[1],F=p(d[3]),G=a(c[1],ed),H=a(M[12],D),I=a(c[1],ee),J=p(E),K=b(c[13],J,I),L=b(c[13],K,H),N=b(c[13],L,G);return b(c[13],N,F)}}function
en(f){var
d=f[2],g=f[1];function
e(b){var
c=a_(g,g,b);return A(a(i[9],c))}switch(d[0]){case
0:return e(d[1]);case
1:var
h=e(d[1]),j=a(c[1],ef);return b(c[13],j,h);case
2:var
k=d[1],l=e(d[2]),m=a(c[1],eg),n=e(k),o=b(c[13],n,m);return b(c[13],o,l);case
3:var
p=d[1],q=e(d[2]),r=a(c[1],eh),s=e(p),t=a(c[1],ei),u=b(c[13],t,s),v=b(c[13],u,r);return b(c[13],v,q);case
4:var
w=d[2],x=d[1],y=e(d[3]),z=a(c[1],ej),B=e(w),C=a(c[1],ek),D=e(x),E=b(c[13],D,C),F=b(c[13],E,B),G=b(c[13],F,z);return b(c[13],G,y);default:var
H=d[2],I=d[1],J=e(d[3]),K=a(c[1],el),L=e(H),M=a(c[1],em),N=e(I),O=b(c[13],N,M),P=b(c[13],O,L),Q=b(c[13],P,K);return b(c[13],Q,J)}}function
bc(c,b,a){return ah}var
bP=cO(eo,ah);function
D(e,a){var
c=a[2][2],f=a[1];if(c){var
d=c[1];return[0,f,[0,b(cu[7],e,d)[1],[0,d]]]}return a}function
bd(o,i){ae([_,function(f){var
d=a1(i),e=a(c[1],ep);return b(c[13],e,d)}]);function
e(a){return D(o,v(a))[2]}function
f(e,d,c){var
f=b(I[16],eq,d),g=[0,a(r[69],f)],h=0,i=0===c?U:[4,y,U,c];return[0,e,[0,av(U,[5,y,g,0,U,i]),h]]}function
k(f,k){if(0===f[0]){var
d=f[1];if(0===d[0])var
b=0;else
var
g=d[1][2],b=1}else
var
b=0;if(!b)var
i=a(c[1],cP),g=h(s[3],0,0,i);var
j=[4,y,[0,[0,[0,[0,y,[0,g]],0],cQ,a3(y)],0],k];return e(aD(y,a3(y),j))[1]}function
N(b){var
c=1-a2(b);return c?bA(a(by[6],b),er):c}var
O=i[2],P=O[2],d=i[1],W=O[1];if(P){var
g=P[1];if(bp===d)return D(o,[0,40,[0,W,[0,g]]]);if(17===g[0]){var
l=g[2];if(R(l,es))if(R(l,et))if(R(l,eu)){if(!R(l,ev)){var
p=g[3],q=p[1];if(q){var
u=q[2];if(u){var
w=u[2];if(w)if(!w[2])if(!p[2])if(!p[3]){var
Q=u[1],X=w[1],Y=q[1];N(Q);var
Z=[0,k(Q,X),0];return f(d,ew,[0,e(Y)[1],Z])}}}}}else{var
x=g[3],z=x[1];if(z){var
A=z[2];if(A)if(!A[2])if(!x[2])if(!x[3]){var
B=A[1],j=z[1];try{var
S=e(j),m=e(B),C=S[1];if(S[2])if(m[2])var
T=m[1],$=a2(j)?f(d,ey,[0,C,[0,T,[0,k(j,B),0]]]):f(d,ez,[0,C,[0,T,0]]),E=$,n=1;else
var
n=0;else
if(m[2])var
n=0;else
var
E=f(d,eB,[0,C,[0,m[1],0]]),n=1;if(!n)var
aa=a(c[1],eA),E=h(s[3],0,0,aa);return E}catch(a){a=t(a);if(a2(j))return f(d,ex,[0,k(j,B),0]);throw a}}}}else{var
F=g[3],G=F[1];if(G){var
H=G[2];if(H){var
J=H[2];if(J)if(!J[2])if(!F[2])if(!F[3]){var
V=H[1],ab=J[1],ac=G[1];N(V);var
ad=[0,k(V,ab),0];return f(d,eC,[0,e(ac)[1],ad])}}}}else{var
K=g[3],L=K[1];if(L){var
M=L[2];if(M)if(!M[2])if(!K[2])if(!K[3]){var
af=L[1],ag=[0,e(M[1])[1],0];return f(d,eD,[0,e(af)[1],ag])}}}}return D(o,i)}return i}function
bQ(b,a){switch(a[0]){case
0:return[0,bd(b,a[1])];case
1:return[1,D(b,a[1])];case
2:var
c=a[1];return[2,c,D(b,a[2])];case
3:var
d=a[1];return[3,d,D(b,a[2])];case
4:var
e=a[2],f=a[1],g=D(b,a[3]);return[4,D(b,f),e,g];default:var
h=a[2],i=a[1],j=D(b,a[3]);return[5,D(b,i),h,j]}}function
E(c,a){var
d=a[1];return[0,d,b(ct[3],c,a[2])]}function
eE(b,a){switch(a[0]){case
0:return[0,E(b,a[1])];case
1:return[1,E(b,a[1])];case
2:var
c=a[1];return[2,c,E(b,a[2])];case
3:var
d=a[1];return[3,d,E(b,a[2])];case
4:var
e=a[2],f=a[1],g=E(b,a[3]);return[4,E(b,f),e,g];default:var
h=a[2],i=a[1],j=E(b,a[3]);return[5,E(b,i),h,j]}}var
P=a(k[2],eF);function
eG(a,b){return[0,a,bQ(a,b)]}b(C[5],P,eG);b(C[6],P,eE);function
eH(d,c){var
e=a(k[5],bP),f=b(k[7],e,c);return b(ac[9],d,f)}b(o[6],P,eH);var
eI=a(k[6],bP),eJ=[0,a(o[2],eI)];b(o[3],P,eJ);var
eK=a(k[4],P),az=h(j[13],j[9],eL,eK),eM=0,eN=0;function
eO(a,b){return[0,v(a)]}var
eP=[0,[0,[0,0,[6,j[15][3]]],eO],eN];function
eQ(a,c,b){return[1,v(a)]}var
eR=[6,j[15][3]],eT=[0,[0,[0,[0,0,[0,a(x[12],eS)]],eR],eQ],eP];function
eU(b,e,a,d){var
c=v(b);return[2,v(a),c]}var
eV=[6,j[15][3]],eX=[0,a(x[12],eW)],eY=[0,[0,[0,[0,[0,0,[6,j[15][3]]],eX],eV],eU],eT];function
eZ(b,f,a,e,d){var
c=v(b);return[3,v(a),c]}var
e0=[6,j[15][3]],e2=[0,a(x[12],e1)],e3=[6,j[15][3]],e5=[0,[0,[0,[0,[0,[0,0,[0,a(x[12],e4)]],e3],e2],e0],eZ],eY];function
e6(c,h,b,g,a,f){var
d=v(c),e=v(b);return[4,v(a),e,d]}var
e7=[6,j[15][3]],e9=[0,a(x[12],e8)],e_=[6,j[15][3]],fa=[0,a(x[12],e$)],fb=[0,[0,[0,[0,[0,[0,[0,0,[6,j[15][3]]],fa],e_],e9],e7],e6],e5];function
fc(c,h,b,g,a,f){var
d=v(c),e=v(b);return[5,v(a),e,d]}var
fd=[6,j[15][3]],ff=[0,a(x[12],fe)],fg=[6,j[15][3]],fi=[0,a(x[12],fh)];h(j[23],az,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,[0,0,[6,j[15][3]]],fi],fg],ff],fd],fc],fb]],eM]]);m(aq[1],P,bc,bc,bc);var
fj=[0,az,0];function
fk(c){var
d=c[2],e=a(k[4],P);return[0,b(k[7],e,d)]}h(ao[5],fl,fk,fj);function
fm(a){return a[1]}function
fn(a){return a}function
aH(j){var
d=j[2],e=d[1],f=d[2];if(f){var
c=f[1];switch(c[0]){case
0:var
g=c[1];if(0===g[0])var
a=0;else
var
b=[0,g[1][2]],a=1;break;case
6:var
h=c[2][2];if(0===h[0])var
a=0;else
if(c[3])var
a=0;else
var
b=[0,h[1][2]],a=1;break;default:var
a=0}}else
if(0===e[0]){var
i=e[1][2];if(0===i[0])var
b=[0,i[1]],a=1;else
var
a=0}else
var
a=0;if(!a)var
b=0;return b?b[1]:bA(V(j),fo)}function
aI(o,n,m){var
d=m[2],p=a(l[8],n),c=d[2],e=d[1];if(c){var
f=c[1],g=r[1][9][1],i=o[1],j=function(c,d,a){return b(r[1][9][4],c,a)},k=h(r[1][10][11],j,i,g);return aj(bx[7],1,p,0,0,[0,[0,k,bx[4][2]]],f)}return e}function
aA(q,p,o){var
e=cz[10],r=o[2],j=a(k[5],e),l=b(k[7],j,r),c=[0,0],m=b(ac[9],q,l);function
f(b){c[1]=[0,b];return a(ap[13],0)}var
g=b(N[4],m,f),h=a(a(ap[66][8],g),p)[2],d=c[1];if(d){var
i=d[1],n=a(k[6],e);return[0,h,b(ac[2][7],n,i)]}throw[0,S,fp]}function
ai(c,b,a){return a1}function
fq(e){var
f=b(i[23],0,e),c=a(bz[17],f);if(typeof
c!=="number"&&0===c[0]){var
d=c[1];if(!R(d,fr))return 40;if(!R(d,fs))return 64}return 32}var
bR=b(j[1][4][5],ft,fq);function
bS(d,c,b){return[0,a(l[2],c),b]}var
w=a(k[2],fu);function
fv(a,b){return[0,a,bd(a,b)]}b(C[5],w,fv);b(C[6],w,E);function
fw(e,d){var
c=[0,function(f){function
g(a){return bS(e,a,d)}var
c=b(l[48][3],g,f),h=c[2],i=c[1],j=a(k[6],w),m=a(o[2],j),n=b(o[1][8],m,h),p=[0,a(N[1],n),i];return a(aT[21][5],p)}];return a(N[8],c)}b(o[6],w,fw);b(o[3],w,0);var
fx=a(k[4],w),aJ=h(j[13],j[9],fy,fx),fz=0,fA=0;function
fB(a,c,b){return v(a)}var
fC=[6,j[15][1]],fE=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(x[12],fD)]],fC],fB],fA]],fz]];h(j[23],aJ,0,fE);m(aq[1],w,ai,ai,ai);var
fF=[0,aJ,0];function
fG(c){var
d=c[2],e=a(k[4],w);return[0,b(k[7],e,d)]}h(ao[5],fH,fG,fF);var
bT=bz[4],fI=0,fJ=0;function
fK(c,b,e){var
d=aw(b,c),f=a(bT,e);if(bf(V(d),f))if(40===b)return aw(bp,c);return d}h(j[1][6],aJ,0,[0,[0,0,0,[0,[0,[0,[2,bR],[0,[2,j[15][1]],0]],fK],fJ]],fI]);var
K=a(k[2],fL);function
fM(a,b){return[0,a,bd(a,b)]}b(C[5],K,fM);b(C[6],K,E);function
fN(e,d){var
c=[0,function(f){function
g(a){return bS(e,a,d)}var
c=b(l[48][3],g,f),h=c[2],i=c[1],j=a(k[6],K),m=a(o[2],j),n=b(o[1][8],m,h),p=[0,a(N[1],n),i];return a(aT[21][5],p)}];return a(N[8],c)}b(o[6],K,fN);var
fO=a(k[6],w),fP=[0,a(o[2],fO)];b(o[3],K,fP);var
fQ=a(k[4],K),aK=h(j[13],j[9],fR,fQ),fS=0,fT=0;function
fU(a,c,b){return v(a)}var
fV=[6,j[15][3]],fX=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(x[12],fW)]],fV],fU],fT]],fS]];h(j[23],aK,0,fX);m(aq[1],K,ai,ai,ai);var
fY=[0,aK,0];function
fZ(c){var
d=c[2],e=a(k[4],K);return[0,b(k[7],e,d)]}h(ao[5],f0,fZ,fY);var
f1=0,f2=0;function
f3(c,b,e){var
d=aw(b,c),f=a(bT,e);if(bf(V(d),f))if(40===b)return aw(bp,c);return d}h(j[1][6],aK,0,[0,[0,0,0,[0,[0,[0,[2,bR],[0,[2,j[15][3]],0]],f3],f2]],f1]);function
f4(l,d,c){var
n=a(r[1][9][5],l),h=b(ar[4][1],d,c),o=b(ar[4][4],d,c),i=[0,a(g[98],d)];try{var
u=a(as[10],h),v=[0,b4(T[53],h,i,u,o,n)],e=v}catch(a){a=t(a);if(a[1]!==T[51])throw a;var
e=0}if(e){var
j=e[1],k=i[1],p=j[2],q=j[1],s=b(ar[4][5],k,c),f=m(ar[4][6],k,q,p,s);return m(ar[4][8],f[3],c,f[1],f[2])}return d}function
bU(F,m,j,E,D){ae([_,function(f){var
d=ah(E),e=a(c[1],f6);return b(c[13],e,d)}]);ae([_,function(i){var
d=a(r[1][10][17],m[1]);function
e(d){var
e=d[1],f=a(o[1][4],d[2][1]),g=a(c[1],f5),h=a(M[12],e),i=b(c[13],h,g);return b(c[13],i,f)}var
f=h(c[53],c[16],e,d),g=a(c[1],f7);return b(c[13],g,f)}]);function
B(b,a){return[2,b,a]}function
al(b,a){return[3,b,a]}function
an(a){return[1,a]}function
p(a,b){var
c=a?a[1]:32;return[0,c,[0,b,0]]}function
w(h,g,o,u,n){try{var
e=aI(h,j,g);switch(e[0]){case
1:var
p=e[1][2];if(b(r[1][10][3],p,h[1]))if(a(ad[3],o))var
f=1;else
if(a(ad[3],F))var
f=1;else
var
v=b(r[1][10][22],p,h[1]),w=a(ad[7],F),x=a(k[6],w),y=b(ac[2][7],x,v),i=b(ad[7],o,y),c=1,f=0;else
var
f=1;if(f)var
c=0;break;case
14:if(13===e[2][0]){var
l=e[3];if(typeof
l==="number")var
d=1;else
if(0===l[0]){var
m=l[1];if(5===m[0]){var
q=m[2];if(q)var
i=b(u,q[1],[0,32,[0,m[5],0]]),c=1,d=0;else
var
c=0,d=0}else
var
c=0,d=0}else
var
d=1;if(d)var
c=0}else
var
c=0;break;default:var
c=0}if(!c)var
i=a(n,g);return i}catch(b){b=t(b);if(a(s[22],b))return a(n,g);throw b}}function
u(c,b,a){return w(m,p(0,c),0,b,a)}function
C(d,i){var
e=a(c[1],d),f=a(c[1],f8),g=b(c[13],f,e);return h(s[3],0,0,g)}function
G(u,k,s,c){var
m=a(e[q],u);if(3===m[0]){var
v=m[1][1],n=a(l[9],j),o=a(at[2][4],n),d=[0,0];try{b(at[2][5],k,n);var
z=function(m){var
e=0===d[1]?1:0;if(e){var
n=b(g[23],c,m),f=a(g[6],n),h=a(at[2][4],f),j=o<h?1:0;if(j){var
p=b(i[17][5],f,(h-o|0)-1|0);d[1]=[0,a(at[2][1][1],p)];var
k=0}else
var
k=j;var
l=k}else
var
l=e;return l},f=d,p=z}catch(a){a=t(a);if(a!==am)throw a;var
f=[0,[0,k]],p=function(a){return 0}}var
w=a(l[2],j),r=function(d,f){var
j=a(e[q],f);if(3===j[0]){var
c=j[1][1];if(!bg(c,v))if(!b(i[17][26],c,d))if(!b(g[26],w,c)){p(c);return[0,c,d]}return d}return h(e[142],r,d,f)},x=r(0,b(T[32],c,s)),y=function(c,d){if(b(g[34],c,d))return c;if(a(ad[3],f[1]))return c;var
e=a(ad[7],f[1]);ae([_,function(b){return a(M[12],e)}]);return f4(e,c,d)};return h(i[17][15],y,c,x)}throw[0,S,f9]}function
H(v){var
b=v[2],f=v[1];switch(b[0]){case
0:var
x=b[1],y=x[2],g=y[1];if(14===g[0])if(13===g[2][0]){var
j=g[3];if(typeof
j==="number")var
d=0;else
if(0===j[0]){var
k=j[1];if(5===k[0]){var
z=k[2];if(z)if(y[2])var
d=1;else{var
e=k[5],A=z[1],D=a(r[68],A),E=8<b6(D)?1:0,J=E?n.caml_string_equal(h(i[15][4],D,0,8),f_):E;if(J){var
c=a(r[68],A),F=h(i[15][4],c,8,b6(c)-8|0);if(R(F,f$)){if(!R(F,ga))if(4===e[0]){var
l=e[3];if(l){var
m=l[2],o=l[1];if(!m)return u(o,B,function(a){return[0,a]});var
q=m[2],G=m[1];if(!q){var
N=function(a){return C(c,a)},O=p(0,o);return u(G,function(a,b){return[4,O,a,b]},N)}if(!q[2]){var
K=q[1],L=function(a){return u(K,B,function(a){throw[0,S,gb]})},M=p(0,o);return u(G,function(a,b){return[4,M,a,b]},L)}}}}else
if(4===e[0]){var
s=e[3];if(s){var
t=s[2];if(t)if(!t[2]){var
P=t[1],Q=s[1],T=function(a){return C(c,a)},U=p(0,Q);return u(P,function(a,b){return[5,U,a,b]},T)}}}return C(c,0)}var
d=1}else
var
d=1}else
var
d=1}else
var
d=0}return w(f,x,[0,H],B,function(a){return[0,a]});case
1:return w(f,b[1],0,al,an);case
2:var
I=b[1],V=b[2],W=function(a){return[2,aH(I),a]};return w(f,V,0,function(a,b){return[4,I,a,b]},W);case
3:var
X=b[2];return[3,aH(b[1]),X];case
4:var
Y=b[3],Z=b[1];return[4,Z,aH(b[2]),Y];default:var
_=b[3],$=b[1];return[5,$,aH(b[2]),_]}}var
d=H([0,m,E]);ae([_,function(g){var
e=bO(d),f=a(c[1],gc);return b(c[13],f,e)}]);if(D){var
x=[0,32,D[1]];switch(d[0]){case
0:var
I=d[1],ao=V(I),v=[0,a4(I,x,function(a,b){return aD(ao,a,b)},av)];break;case
2:var
aB=d[2],aC=d[1],v=[5,p(0,av(U,aI(m,j,x))),aC,aB];break;case
4:var
aj=d[3],aE=d[2],aF=d[1],aG=p(0,aI(m,j,x)),aJ=V(aj),v=[4,a4(aF,aG,function(a,b){return aD(aJ,a,b)},av),aE,aj];break;case
5:var
ak=d[3],aK=d[2],aL=d[1],aM=p(0,aI(m,j,x)),aN=V(ak),v=[5,a4(aL,aM,function(a,b){return aD(aN,a,b)},av),aK,ak];break;default:var
v=d}var
f=v}else
var
f=d;ae([_,function(g){var
d=bO(f),e=a(c[1],gd);return b(c[13],e,d)}]);function
J(a,b,c){var
d=c[2],e=d[2],f=d[1],g=c[1];if(e){var
h=e[1];return[0,g,[0,f,[0,[5,a,[0,a,b],a3(a),h]]]]}return[0,g,[0,[7,a,b,[13,[0,a,[1,b],0,0]],f],0]]}switch(f[0]){case
0:var
K=aA(m,j,f[1]);return[0,K[1],[0,K[2]]];case
1:var
L=aA(m,j,f[1]);return[0,L[1],[1,L[2]]];case
2:case
3:var
N=f[1],O=aA(m,j,J(y,[0,N],f[2])),ap=O[1],P=a(e[36],O[2]),Q=P[4],z=P[2],W=G(z,N,Q,ap),aq=b(T[32],W,Q),X=b(aQ[13],z,aq),ar=2===f[0]?[2,z,X]:[3,z,X];return[0,W,ar];default:var
Y=f[2],as=f[1],Z=aA(m,j,J(y,[0,Y],f[3])),au=Z[1],$=a(e[36],Z[2]),aa=$[4],A=$[2],ab=G(A,Y,aa,au),aw=b(T[32],ab,aa),af=b(aQ[13],A,aw),ax=a(g[68],j),ag=aA(m,b(l[3],ax,ab),as),ai=ag[2],ay=ag[1],az=4===f[0]?[4,ai,A,af]:[5,ai,A,af];return[0,ay,az]}}function
bV(d,c,b,a){return bU(0,d,c,[0,b],a)}function
ge(d){var
b=d[2];if(0===b[0]){var
c=a(e[q],b[1]);return 1===c[0]?[0,c[1]]:0}return 0}function
bW(v,i,j,o,n,r,p){function
f(c,a){return b(H[19],c,a)}function
w(e,d,l){var
f=b(g[23],e,d),i=f[3];if(i)var
j=i[1];else
var
n=a(c[1],gf),o=a(c[1],gg),p=a(c[16],0),q=a(bw[1],d),r=a(c[19],q),s=a(c[1],gh),t=a(aC,l),u=a(c[1],gi),v=b(c[13],u,t),w=b(c[13],v,s),x=b(c[13],w,r),y=b(c[13],x,p),z=b(c[13],y,o),j=a(O,b(c[13],z,n));var
k=f.slice();k[3]=0;var
m=b(g[25],e,d);return[0,h(g[22],m,d,k),j]}function
x(c){var
b=a(e[q],c);if(3===b[0])return b[1][1];throw[0,S,gj]}function
l(j,i,h,b,a,g){var
c=b[2],d=b[1],k=a?a[1]:c,e=bK(0,j,i,h,[0,d,k],g,0,f(d,c));return[0,e[1],[0,e[2],0]]}if(n){var
D=n[1],k=D[2],d=D[1];switch(k[0]){case
4:var
M=k[2],ag=k[3],ah=f(d,k[1]),t=f(d,ag),ai=x(M),N=B(0,0,0,j,Z,l(gl,i,j,[0,d,t],0,J)),aj=N[2],ak=N[1],P=B(0,0,0,d,Z,l(0,i,d,[0,d,M],0,J)),al=P[2],am=P[1],Q=B(0,v,0,j,r,l(0,i,j,[0,d,ah],0,J)),an=Q[2],ao=Q[1],ap=m(ak,i,o,1,function(b,i,n,h){var
c=Y(b,a(g[$],d),i,t),e=w(c,ai,t),j=e[2],k=e[1];function
l(b,d,c,a){return m(ao,b,j,a,p)}return f(c,m(am,b,f(k,t),h,l))});a(an,0);a(al,0);a(aj,0);return ap;case
5:var
y=k[2],aq=k[3],z=f(d,k[1]),u=f(d,aq),ar=x(y),R=Y(i,d,y,z),T=B(0,0,0,j,Z,l(gm,i,j,[0,R,f(R,u)],0,J)),as=T[2],at=T[1],U=B(0,0,0,d,r,l(0,i,d,[0,d,y],0,J)),au=U[2],av=U[1],aw=m(at,i,o,1,function(b,j,n,i){var
c=Y(b,a(g[$],d),j,u),e=w(c,ar,u),h=e[1],k=e[2];function
l(a,e,d,c){var
b=f(Y(a,h,k,z),z);return m(p,a,b,b,c)}return f(c,m(av,b,f(h,u),i,l))});a(au,0);a(as,0);return aw;case
0:case
1:var
V=f(d,k[1]),W=a(g[$],d);if(n)if(0===n[1][2][0])var
E=r,A=1;else
var
A=0;else
var
A=0;if(!A)var
E=Z;var
F=B(0,v,0,j,E,l(0,i,j,[0,W,V],0,J)),X=F[2],_=m(F[1],i,o,1,p);a(X,0);return _;default:var
G=k[1],s=f(d,k[2]);if(n)if(2===n[1][2][0])var
I=r,C=1;else
var
C=0;else
var
C=0;if(!C)var
I=Z;var
aa=x(G),K=B(0,0,0,j,Z,l(gk,i,j,[0,d,s],0,J)),ab=K[2],ac=K[1],L=B(0,v,0,d,I,l(0,i,d,[0,d,G],0,J)),ad=L[2],ae=L[1],af=m(ac,i,o,1,function(c,j,o,i){var
e=Y(c,a(g[$],d),j,s),h=w(e,aa,s),k=h[2],l=h[1];function
n(a,c){return b(p,a,k)}return f(e,m(ae,c,f(l,s),i,n))});a(ad,0);a(ab,0);return af}}return m(p,i,o,o,1)}function
bX(e,l,d){var
f=d[2],i=d[1],m=e?e[1]:0;switch(f[0]){case
1:case
3:var
o=a(c[1],go),j=h(s[3],0,0,o);break;default:var
j=f[1]}var
k=m?b5(bq[29],0,0,0,0,gn,l,i):i,n=a(g[q],k);return[0,b(H[19],k,j),n]}function
bY(m,f,l,k,d,c,j){if(bg(c,gp))var
i=0,h=gq;else
var
i=1,h=c;var
b=[0,0],n=bW(m,f,l,k,[0,d],h,function(h,c,f,d){ba(b,function(a){return[0,c,g[b_]]});return i?a(e[aO],(d+j|0)-1|0):c}),o=0===b[1]?bX(0,f,d):bb(b);return[0,o,n]}function
be(g,f,e,d,c,b,a){return bK(g,0,f,e,d,c,b,a)}function
gr(f,p,o,d,n,c,l,k){var
q=c[2],h=be(0,f,d,[0,a(g[$],c[1]),q],l,0,n),i=B(0,gs,0,d,o,[0,h[1],[0,h[2],0]]),r=i[2],s=i[1],t=m(s,f,p,k,function(c,b,a){return e[aO]}),j=a(r,0),b=j[3];return[0,b[1],b[2],b[3],t,j[1]]}function
gt(k,j,o,d,i){var
e=i[2],l=i[1];try{var
f=gr(k,j,o,d,e,[0,l,e],J,1),n=f[1],B=f[4],C=f[3],D=f[2],E=n!==d?a(s[6],gw):[0,B,[0,b(g[bl],n,D),C]];return E}catch(f){f=t(f);if(f===z)try{var
v=function(a){return 1},h=aF(k,d,a(g[$],l),e,v),m=h[1],w=h[3],x=h[2];if(m!==d)throw z;var
y=[0,j,[0,b(g[bl],m,x),w]];return y}catch(d){var
p=a(c[1],gu),q=A(e),r=a(c[1],gv),u=b(c[13],r,q);return a(O,b(c[13],u,p))}throw f}}function
gx(b,e,d){var
f=a(l[2],b),g=a(l[8],b),c=gt(g,a(l[7],b),e,f,d);return[0,c[1],c[2][2]]}function
gy(a){return[0,32,[0,[0,[0,y,[0,a],0]],0]]}function
gz(c){var
a=c[2],b=a[2],d=a[1],e=b?12===b[1][0]?1:0:13===d[0]?1:0;return e?1:0}function
gA(d,c,b,a){return ah(a[2])}function
bZ(d,c,b,a){return ah(a)}var
F=a(k[2],gB);function
gC(a,b){return[0,a,bQ(a,b)]}b(C[5],F,gC);function
gD(b,a){return a}b(C[6],F,gD);function
gE(e,d){var
c=[0,function(f){function
g(b){return[0,a(l[2],b),[0,e,d]]}var
c=b(l[48][3],g,f),h=c[2],i=c[1],j=a(k[6],F),m=a(o[2],j),n=b(o[1][8],m,h),p=[0,a(N[1],n),i];return a(aT[21][5],p)}];return a(N[8],c)}b(o[6],F,gE);b(o[3],F,0);b(j[11],F,az);m(aq[1],F,bZ,bZ,gA);var
gF=[0,az,0];function
gG(c){var
d=c[2],e=a(k[4],F);return[0,b(k[7],e,d)]}h(ao[5],gH,gG,gF);function
gI(d,c){var
e=a(l[2],c),f=b(g[bl],e,d),h=a(g[68],c);return b(l[3],h,f)}function
gJ(d,c){var
e=a(l[2],c),f=b(g[bh],e,d),h=a(g[68],c);return b(l[3],h,f)}function
b0(c,b,a){return bU([0,F],c,b,a,0)}var
gM=[0,function(o,c){var
d=c[1],f=a(r[1][5],gL),i=b(r[1][10][22],f,d),m=a(k[6],F),j=b(ac[2][7],m,i);function
n(c){var
p=b0(j[1],c,j[2]),q=a(l[2],c),s=a(l[7],c),f=bY(0,a(aX[2],0),q,s,p,Z,1),i=f[1][1],t=f[2],d=b(l[16],c,i),k=d[2],m=d[1],n=a(g[68],c),o=b(l[3],n,m),u=[0,[0,a(r[69],gK)],i,k,t],v=a(e[co],u),w=h(cr[3],0,v,2);return b(ap[66][8],w,o)}return a(ap[66][1],n)}];h(aS[9],0,b1,gM);var
gN=[31,aW[4],[0,b1,0],0],gP=[28,[0,[0,[0,a(r[1][5],gO)],0],gN]];function
gQ(c){var
b=a(r[1][5],gR);return m(aS[4],1,0,b,gP)}b(aV[19],gQ,gS);function
gT(n,k,d){var
o=a(l[7],d),e=a(l[2],d),f=a(l[8],d),g=bV(n,d,k,0),h=g[2],p=g[1],i=0===h[0]?h[1]:a(O,a(c[1],g0)),q=0,j=be(0,f,e,[0,p,i],function(a,b){return 1},q,i),r=B(gV,gU,0,e,0,[0,j[1],[0,j[2],0]])[1];function
s(t,e,d,s){var
f=a(aC,d),g=a(c[16],0),h=a(c[1],gW),i=a(c[16],0),j=a(aC,e),k=a(c[16],0),l=a(c[1],gX),m=b(c[13],l,k),n=b(c[13],m,j),o=b(c[13],n,i),p=b(c[13],o,h),q=b(c[13],p,g),r=b(c[13],q,f);b(aZ,0,b(c[29],1,r));return d}b(aZ,0,a(c[1],gY));try{for(;;){m(r,f,o,1,s);continue}}catch(e){e=t(e);if(e===z){b(aZ,0,a(c[1],gZ));return a(cs[1],d)}throw e}}var
g1=0,g3=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(k[6],w),f=b(ac[2][7],e,d);return function(b){function
c(a){return gT(b,f,a)}return a(ap[66][1],c)}}return a(I[2],g2)},g1],g4=a(i[19][12],g3);h(aS[9],0,[0,aY,g5],g4);function
g6(f){var
c=0,d=0,e=a(r[1][6],g7);if(0===w[0])return b(ao[4],[0,aY,g_],[0,[0,g9,[0,[1,aW[4],[5,[0,w[1]]],e],d]],c]);throw[0,S,g8]}b(aV[19],g6,aY);a(x[9],cC);var
b2=[0,a1,aJ,w,aK,K,ah,az,P,z,af,en,bX,b0,bV,bW,bY,ag,be,B,gx,fn,ba,bb,Y,dh,fm,V,ge,gz,gy,A,gI,gJ,bB,bF];b7(251,b2,"Ssrmatching_plugin.Ssrmatching");b7(252,[0,b2],"Ssrmatching_plugin");return});

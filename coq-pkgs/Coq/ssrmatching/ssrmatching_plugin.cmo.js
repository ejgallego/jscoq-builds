(function(hj){"use strict";var
ca="Only identifiers are allowed here",cj=141,cr=123,r=140,aO=112,cq="partial term ",cg="mk_tpattern_matcher with no upats_origin",aa="in",cf="do_once never called",cp="(",ce="ssrinstoftpat",bm="rpattern",al="In",ci="ssrpattern",co="pattern",L=124,$=246,b$=111,bk="As",u=" in ",bq=120,b_="_ssrpat_",cd=113,cn="The ",aM="!! ",cc=122,ak="in ",bp="cpattern",aN=143,bo="lcpattern",cm="!! %-39s %10d %9.4f %9.4f %9.4f",b9="ssrpatternarg",ch=109,cl="total",U="plugins/ssrmatching/ssrmatching.ml4",aP="ssrmatching_plugin",G=136,bn=248,ck="Qed",cb=" of ",bj=147,bi=146,ab=174,bl=" as ",o=hj.jsoo_runtime,S=o.caml_check_bound,bh=o.caml_equal,aL=o.caml_fresh_oo_id,b7=o.caml_ml_string_length,d=o.caml_new_string,bg=o.caml_notequal,b8=o.caml_register_global,b4=o.caml_string_get,T=o.caml_string_notequal,t=o.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):o.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):o.caml_call_gen(a,[b,c])}function
h(a,b,c,d){return a.length==3?a(b,c,d):o.caml_call_gen(a,[b,c,d])}function
m(a,b,c,d,e){return a.length==4?a(b,c,d,e):o.caml_call_gen(a,[b,c,d,e])}function
b5(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):o.caml_call_gen(a,[b,c,d,e,f])}function
aj(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):o.caml_call_gen(a,[b,c,d,e,f,g])}function
b6(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):o.caml_call_gen(a,[b,c,d,e,f,g,h])}var
f=o.caml_get_global_data(),he=[0,4],hf=[0,1,9],hg=[0,1,9],hh=[0,4],hi=[0,1,9],aQ=d(aP),_=[0,[0,0,0]],b2=[0,d(aP),d(ci)],P=f.Assert_failure,n=f.Names,aS=f.Loc,ax=f.Tacentries,ay=f.Proofview,j=f.Genarg,ag=f.Tacinterp,H=f.Pervasives,c=f.Pp,l=f.Tacmach,bf=f.Tacenv,aW=f.Global,e=f.Term,g=f.Evd,q=f.Geninterp,O=f.Ftactic,bd=f.Sigma,s=f.CErrors,bE=f.Typeclasses,J=f.Reductionops,bM=f.Evar,i=f.Util,N=f.Ppconstr,ai=f.Option,au=f.Context,an=f.Not_found,Y=f.Evarutil,a_=f.Vars,aA=f.Goal,ao=f.Environ,bR=f.Compat,bz=f.Constrexpr_ops,bN=f.Termops,bG=f.Recordops,bC=f.Evarconv,a5=f.Unification,aE=f.Printf,as=f.Unix,C=f.Genintern,ap=f.Pptactic,aV=f.Printer,bw=f.Constrintern,bs=f.Feedback,x=f.CLexer,aR=f.Mltop,bu=f.Goptions,k=f.Pcoq,g3=f.Tacticals,gN=f.Tactics,fr=f.Constrarg,eE=f.Tacsubst,eo=f.Tacintern,dN=f.CArray,dE=f.Failure,dy=f.Pretype_errors,dt=f.Invalid_argument,dn=f.Globnames,c7=f.Reduction,cK=f.Glob_ops,cu=f.CamlinternalLazy,cs=a(x[8],0);a(aR[12],d(aP));var
y=aS[4],M=a(s[7],d("ssrmatching")),ha=[0,d(U),1,0],g$=d("$arg"),hb=[0,d("ssrinstancesoftpat")],hc=d(ce),g6=d("Extension: cannot occur"),gZ=d("matches:"),g0=d("instance:"),g4=d("Not supported"),gX=[0,1],gY=[0,1],g1=d("BEGIN INSTANCES"),g2=d("END INSTANCES"),gU=d(ci),gO=d(co),gM=d("selected"),gy=d("matching impacts evars"),gw=d(" does not match any subterm of the goal"),gx=d(cq),gu=[0,1],gr=[0,[0,1,0]],gs=[0,[0,0,[0,1,0]]],gq=d("pattern without redex"),gp=[0,0],gl=[0,d(U),1241,56],gh=d("in the pattern?"),gi=d('Does the variable bound by the "in" construct occur '),gj=d(" did not instantiate ?"),gk=d("Matching the pattern "),gn=[0,1],go=[0,1],gm=[0,1],gf=d("typed as: "),ge=d("decoded as: "),gd=[0,d(U),1167,54],ga=d(b_),gb=d(bk),gc=d(al),f$=[0,d(U),1127,63],f_=d("bad encoding for pattern "),f9=d(" in ist: "),f8=d("interpreting: "),f7=d(":"),ft=d(cp),fu=d("@"),fq=[0,d(U),1025,12],fp=d(ca),er=d(ca),eq=d(b_),ep=d("globbing pattern: "),es=d("( _ as _ )"),et=d("( _ as _ in _ )"),eu=d("( _ in _ )"),ev=d("( _ in _ in _ )"),ew=d(al),ey=d(al),ez=d(al),eB=d(al),eA=d("where are we?"),ex=d(al),eC=d(bk),eD=d(bk),ee=d(ak),ef=d(u),eg=d(u),eh=d(ak),ei=d(u),ej=d(u),ek=d(u),el=d(bl),d8=d(ak),d9=d(u),d_=d(u),d$=d(ak),ea=d(u),eb=d(u),ec=d(u),ed=d(bl),d0=d(ak),d1=d(u),d2=d(u),d3=d(ak),d4=d(u),d5=d(u),d6=d(u),d7=d(bl),dY=d("matches but type classes inference fails"),dZ=d("does not match any subterm of the goal"),dX=d(cg),dV=d("are equal to the "),dW=d("all matches of "),dU=d("companion function never called"),dO=d("of "),dP=d(" of the "),dT=d(" of"),dQ=d(" occurence"),dR=d(" < "),dS=d("Only "),dI=d(cb),dJ=d(cn),dG=d(cb),dH=d(cn),dL=d("term "),dM=d(cq),dK=d(cg),dF=d(cf),dC=d(cf),ds=d("incomplete ise in match_upats_FO"),du=d("IN FO"),dp=[0,d(U),533,13],dj=d(u),dk=d("indeterminate "),dl=d("indeterminate pattern"),dd=d("RHS"),dc=d("LHS"),c$=[0,1],c5=[0,[11,d(aM),[2,[0,0,39],[12,32,[4,0,[0,1,10],0,[12,32,[8,0,[0,1,9],[0,4],[12,32,[8,0,[0,1,9],[0,4],[12,32,[8,0,hf,he,0]]]]]]]]]],d(cm)],c4=[0,d(U),214,26],cW=[0,[11,d(aM),[2,[0,1,39],[11,d(" ---------- --------- --------- ---------"),0]]],d("!! %39s ---------- --------- --------- ---------")],cX=d("average"),cY=d("max"),cZ=d(cl),c0=d("#calls"),c1=d("function"),c2=[0,[11,d(aM),[2,[0,0,39],[12,32,[2,[0,1,10],[12,32,[2,[0,1,9],[12,32,[2,[0,1,9],[12,32,[2,hg,0]]]]]]]]]],d("!! %-39s %10s %9s %9s %9s")],cT=[0,d(U),207,26],cQ=d(cl),cR=[0,[11,d(aM),[2,[0,0,39],[12,32,[4,0,[0,1,10],0,[12,32,[8,0,[0,1,9],[0,4],[12,32,[8,0,[0,1,9],[0,4],[12,32,[8,0,hi,hh,0]]]]]]]]]],d(cm)],cI=d("have: mixed C-G constr"),cJ=d("have: mixed G-C constr"),cH=[0,0],cG=d("not a CRef"),cA=d("$"),cy=d(")"),cz=d(cp),ct=d("SSR: "),hd=d("SSRMATCHINGDEBUG"),cv=[0,d("Debug"),[0,d("SsrMatching"),0]],cw=d("ssrmatching debugging"),cM=[0,d("SsrMatchingProfiling"),0],cN=d("ssrmatching profiling"),c6=d("Ssrmatching.NoProgress"),c8=d("unif_EQ_args"),db=d("Ssrmatching.NoMatch"),dg=d("_"),dq=d("Ssrmatching.FoundUnif"),dv=d("match_upats_FO"),dz=d("match_upats_HO"),en=d("rpatternty"),eG=d(bm),eM=d(bm),eT=d(aa),eX=d(aa),e2=d(aa),e5=d(aa),e9=d(aa),fa=d(aa),ff=d(aa),fi=d("as"),fm=d(bm),fv=d("ssrtermkind"),fw=d(bp),fA=d(bp),fF=d(ck),fJ=d(bp),fN=d(bo),fT=d(bo),fY=d(ck),f2=d(bo),gD=d(b9),gJ=d(b9),gR=d(co),gV=d(aP),g9=d(ce);function
br(d,b){var
e=[0,d,b,a(c[1],b)];return a(s[8],e)}var
aT=bs[12],am=[0,function(a){return 0}];function
aU(d){var
e=o.caml_obj_tag(d),f=250===e?d[1]:$===e?a(cu[2],d):d,g=a(c[1],ct),h=b(c[13],g,f);return b(bs[16],0,h)}try{o.caml_sys_getenv(hd);am[1]=aU}catch(a){a=t(a);if(a!==an)throw a}function
bt(a){return a?(am[1]=aU,0):(am[1]=function(a){return 0},0)}var
cx=[0,0,0,cw,cv,function(a){return am[1]===aU?1:0},bt];b(bu[4],0,cx);function
ac(b){return a(am[1],b)}function
bv(c){var
b=a(e[r],c);return 9===b[0]?[0,b[1],b[2]]:[0,c,[0]]}function
bx(e,d,c){var
a=b4(d,c);if(48<=a)var
b=61===a?1:cr===a?1:0;else{if(40===a)return 0;var
b=47<=a?1:0}return b?1:40===e?1:0}function
by(m,f,e){var
n=a(f,e),o=a(c[42],n),g=b(H[16],o,cA),d=0;for(;;){if(22<(b4(g,d)-10|0)>>>0){if(b(m,g,d)){var
h=a(c[1],cy),i=a(f,e),j=a(c[1],cz),k=b(c[13],j,i),l=b(c[13],k,h);return b(c[29],1,l)}return a(f,e)}var
d=d+1|0;continue}}var
aB=aV[5],cB=N[24],cC=N[23];function
cD(c){var
d=c[2],f=c[1];if(d)return a(cB,d[1]);var
e=a(aW[2],0);return b(aV[28],e,f)}function
cE(c){var
d=c[2],f=c[1];if(d)return a(cC,d[1]);var
e=a(aW[2],0);return b(aV[30],e,f)}function
aX(a){var
b=a[2],c=a[1];return by(function(a,b){return bx(c,a,b)},cE,b)}function
p(a){var
b=a[2],c=a[1];return by(function(a,b){return bx(c,a,b)},cD,b)}function
cF(e,g){var
c=a(j[2],e),f=a(q[1][1],e);function
h(b,a){return[0,b,a]}function
i(b,a){return a}function
k(c,b){return a(O[1],[0,f,b])}function
d(c,b,a){return g}b(C[5],c,h);b(C[6],c,i);b(q[6],c,k);b(q[3],c,[0,[0,f]]);m(ap[1],c,d,d,d);return c}function
aY(a){if(0===a[0])if(0!==a[1][0])return 1;return 0}function
aZ(a){return[12,a,0,0,0]}function
aC(c,b,a){return[16,c,b,[0,a]]}var
V=[13,[0,y,0,0,0]];function
aq(b,a){return[14,y,b,[0,a]]}function
a0(e,d,m,l){var
f=e[2],g=f[2],i=e[1],n=f[1];if(g){var
j=d[2][2];if(j)return[0,i,[0,V,[0,b(m,g[1],j[1])]]];var
o=a(c[1],cI);return h(s[3],0,0,o)}var
k=d[2];if(k[2]){var
p=a(c[1],cJ);return h(s[3],0,0,p)}return[0,i,[0,b(l,n,k[1]),0]]}function
W(d){var
b=d[2],c=b[2],e=b[1];return c?a(bz[6],c[1]):a(cK[15],e)}function
ar(b,a){return[0,b,[0,V,[0,a]]]}var
cL=32;function
v(a){return ar(cL,a)}var
a1=[0,0],a2=[0,0],aD=[0,0];function
a3(a){aD[1]=[0,a,aD[1]];return 0}function
bA(c){a1[1]=c;if(c){var
e=aD[1],f=function(b){return a(b[2],0)};b(i[17][11],f,e)}var
d=1-c;if(d){var
g=aD[1],h=function(b){return a(b[3],0)};return b(i[17][11],h,g)}return d}var
cO=[0,0,0,cN,cM,function(a){return a1[1]},bA];b(bu[4],0,cO);var
bB=[0,0];function
cP(f){var
b=a2[1];if(b){var
c=bB[1],d=a(as[87],0)-c,e=aj(aE[4],cR,cQ,0,d,0,0);return a(H[38],e)}return b}function
cS(b){bB[1]=a(as[87],0);return 0}var
cU=[0,function(b,a){throw[0,P,cT]},cS,cP];function
cV(g){var
c=a2[1];if(c){var
d=b(i[15][1],39,45),e=b(aE[4],cW,d);a(H[38],e);var
f=aj(aE[4],c2,c1,c0,cZ,cY,cX);return a(H[38],f)}return c}function
c3(a){return 0}a3([0,function(b,a){throw[0,P,c4]},c3,cV]);a3(cU);function
a4(f){var
c=[0,0],d=[0,0],b=[0,0];function
g(a){b[1]=0;d[1]=0;c[1]=0;return 0}function
h(h,g){if(a1[1]){var
i=a(as[87],0);try{d[1]++;var
j=a(h,g),f=a(as[87],0)-i;b[1]=b[1]+f;if(c[1]<f)c[1]=f;return j}catch(d){d=t(d);var
e=a(as[87],0)-i;b[1]=b[1]+e;if(c[1]<e)c[1]=e;throw d}}return a(h,g)}var
e=[0,h,g,function(h){var
e=0!==d[1]?1:0;if(e){a2[1]=1;var
g=aj(aE[4],c5,f,d[1],b[1],c[1],b[1]/d[1]);return a(H[38],g)}return e}];a3(e);return e}var
ad=[bn,c6,aL(0)];function
at(e,b,d,c){var
f=a(g[145],b),h=[0,a(g[42],b),f];try{aj(c7[12],0,0,e,[0,h],d,c);var
i=1;return i}catch(a){return 0}}var
c9=a4(c8);function
X(d,c,b,a){return b5(bC[2],d,0,b,a,c)}function
bD(h,g,c,f,e){var
b=g,a=0,i=c.length-1;for(;;){if(a===i)return b;var
d=f+a|0,j=S(e,d)[d+1],b=X(h,b,S(c,a)[a+1],j),a=a+1|0;continue}}var
Q=a(a5[4],0)[1],a6=[0,0,Q[2],Q[3],Q[4],n[61],Q[6],Q[7],Q[8],Q[9],Q[10],1,1],c_=[0,a6,a6,a6,0,a(a5[4],0)[5]];function
a7(d,c,b,a){return aj(a5[8],d,c,0,[0,c_],b,a)}function
a8(j,d,k){var
c=[0,j];function
f(l){var
m=a(e[r],l);if(3===m[0]){var
k=m[1];try{var
q=f(b(g[40],d,k));return q}catch(l){var
j=k[1],n=b(i[19][15],f,k[2]);if(1-b(g[26],c[1],j)){var
o=b(g[23],d,j),p=b(Y[40],d,o);c[1]=h(g[22],c[1],j,p)}return a(e[115],[0,j,n])}}return b(e[aN],f,l)}function
l(e,j,n){if(0===a(g[10],j)){var
k=b(g[23],d,e),i=a(g[10],k);if(i){var
l=c[1],m=f(i[1]);c[1]=h(g[31],e,m,l);return 0}return 0}return 0}var
m=f(k);h(g[27],l,j,0);var
n=a(g[cj],d);return[0,c[1],n,m]}function
aF(i,f,q,p,o){var
j=h(bC[6],i,0,q),c=a8(f,j,p),k=c[3],d=c[2],l=c[1],r=a(g[ab],l),m=b(g[bj],r,d),n=b6(bE[29],0,0,0,0,c$,i,m);if(a(o,j)){if(n===m)return[0,l,d,k];var
e=a8(f,n,k),s=e[3],t=e[1];return[0,t,b(g[aO],d,e[2]),s]}throw ad}function
Z(d,c,f,a){var
h=X(d,c,f,a),e=aF(d,c,h,a,function(a){return 1});return b(g[bj],e[1],e[2])}function
da(c,e,d){var
f=a(g[68],c),h=a(l[2],c),i=Z(a(l[8],c),h,e,d);return b(l[3],f,i)}function
bF(c,k){var
d=a(e[r],k);switch(d[0]){case
3:return b(i[19][13],c,d[1][2]);case
5:var
l=d[3];a(c,d[1]);return a(c,l);case
8:var
n=d[4],o=d[3];a(c,d[2]);a(c,o);return a(c,n);case
9:var
p=d[2];a(c,d[1]);return b(i[19][13],c,p);case
13:var
q=d[4],s=d[2];a(c,d[3]);a(c,s);return b(i[19][13],c,q);case
16:return a(c,d[2]);case
6:case
7:var
m=d[3];a(c,d[2]);return a(c,m);case
14:case
15:var
g=d[1][2],h=g[2],j=h.length-1-1|0,t=g[3],u=0;if(!(j<0)){var
f=u;for(;;){a(c,S(h,f)[f+1]);a(c,S(t,f)[f+1]);var
v=f+1|0;if(j!==f){var
f=v;continue}break}}return 0;default:return 0}}var
z=[bn,db,aL(0)];function
ae(b){return 0===b?a(c[1],dc):a(c[1],dd)}function
de(a){return 0===a?1:0}function
I(b,a){return 1}function
df(b){try{var
c=1+a(bG[4],[1,b])|0;return c}catch(a){return 0}}function
bH(b){switch(a(e[r],b)[0]){case
4:case
6:case
7:case
13:case
14:case
15:return 1;default:return 0}}aL(0);var
dh=a(n[69],dg),di=a(e[cd],dh);function
A(d){function
c(d){return a(e[6],d)?di:b(e[aN],c,d)}return a(aB,c(d))}function
bI(F,E,Z,D,C,X,W,V){var
w=C[1],_=C[2],$=E?E[1]:0,G=b(J[30],w,V),d=G[2],f=G[1],o=a(e[r],f);switch(o[0]){case
3:var
K=o[1][1];if(b(g[26],D,K))var
u=[0,[0,K],f,d];else
if(0===d)if(F)var
N=F[1],ab=N[1],ac=A(N[2]),ad=a(c[1],dj),af=ae(ab),ag=a(c[1],dk),ah=b(c[13],ag,af),ai=b(c[13],ah,ad),u=a(M,b(c[13],ai,ac));else
var
u=a(s[6],dl);else
var
u=[0,5,f,d];var
k=u,l=0;break;case
7:var
k=[0,3,f,d],l=0;break;case
8:var
aj=o[4],ak=o[2],al=bg(aj,a(e[aO],1))?[0,2,f,d]:[0,5,ak,d],k=al,l=0;break;case
10:var
O=o[1][1],z=df(O);if(0===z)var
B=0;else
if(a(i[17][1],d)<z)var
B=0;else
var
Q=b(i[17][99],z,d),am=Q[2],P=[0,[1,O],a(e[59],[0,f,Q[1]]),am],B=1;if(!B)var
P=[0,1,f,d];var
k=P,l=0;break;case
16:var
k=[0,[1,a(n[ch][3],o[1])],f,d],l=0;break;case
1:case
11:case
12:var
y=0,q=f,x=d,l=1;break;default:var
y=4,q=f,x=d,l=1}if(!l)var
y=k[1],q=k[2],x=k[3];var
I=a(i[19][12],x),v=[0,w],p=[0,w],aa=a(e[L],[0,q,I]),S=$?1:0,R=a(ao[9],Z),T=a(i[17][1],R)+S|0;function
j(q){var
c=q;for(;;){var
f=a(e[r],c);if(3===f[0]){var
d=f[1],k=d[1],s=d[2];try{var
G=j(b(g[40],p[1],d));return G}catch(f){f=t(f);if(f===g[39]){if(b(g[26],D,k))return b(e[aN],j,c);var
l=b(g[23],p[1],k),u=a(g[7],l),w=b(H[5],0,s.length-1-T|0),x=b(i[17][104],w,u),y=function(c,b){var
d=c[2],f=c[1];if(0===b[0]){var
g=b[1],i=j(b[2]),k=h(e[52],g,i,d);return[0,[0,a(e[cd],g),f],k]}var
l=b[2],n=b[1],o=j(b[3]),p=j(l);return[0,f,m(e[51],n,p,o,d)]},z=[0,0,j(l[1])],n=h(au[2][9],y,z,x),A=n[2],B=n[1],o=a(Y[1],0);v[1]=m(g[95],o,A,0,v[1]);var
C=p[1],E=[0,a(e[114],o),B],F=a(e[59],E);p[1]=h(g[31],k,F,C);var
c=b(g[40],p[1],d);continue}throw f}}return b(e[aN],j,c)}}var
U=j(aa);return[0,v[1],[0,y,U,q,I,_,W,X]]}function
bJ(h,d,f){var
i=d[1],n=d[3],o=d[2],j=bv(h),k=j[1],p=j[2],l=a(e[r],k);switch(l[0]){case
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
dm(j,h,m){function
f(b){var
c=a(bG[8],[0,[1,j],b])[2][7];return a(i[17][1],c)}try{var
g=a(e[r],h);switch(g[0]){case
4:var
d=f([1,a(e[b$],g[1])]),c=1;break;case
6:var
d=f(0),c=1;break;case
10:if(b(n[17][13],g[1][1],j)){var
k=a(e[r],m[3]);switch(k[0]){case
9:var
l=k[2].length-1;break;case
16:var
l=0;break;default:throw[0,P,dp]}var
d=l,c=1}else
var
c=0;break;case
1:case
11:case
12:var
c=0;break;default:var
d=-1,c=1}if(!c)var
d=f([0,a(dn[16],h)]);return d}catch(a){a=t(a);if(a===an)return-1;throw a}}function
bK(d,c){var
b=a(e[r],c);return 3===b[0]?bh(d,b[1][1]):0}function
aG(d,c,b){if(0===c)return d;var
f=c===b.length-1?b:h(i[19][7],b,0,c);return a(e[L],[0,d,f])}function
a9(j){function
h(l,k){var
d=l,c=k;for(;;){var
f=a(e[r],d);switch(f[0]){case
3:var
m=f[1];try{var
n=h(b(g[40],j,m),c);return n}catch(a){return[0,d,c]}case
5:var
d=f[1];continue;case
9:var
o=f[1],d=o,c=b(i[19][5],f[2],c);continue;default:return[0,d,c]}}}return function(b){var
c=a(e[r],b);switch(c[0]){case
9:return h(c[1],c[2]);case
3:case
5:return h(b,[0]);default:return[0,b,[0]]}}}var
av=[bn,dq,aL(0)];function
bL(c){var
d=a(g[84],c);return function(a){function
c(c){try{b(g[24],a,c);var
d=1;return d}catch(a){a=t(a);if(a===an)return 0;throw a}}return b(bM[6][16],c,d)}}function
dr(p,k,y,j,d){var
A=bL(d);function
f(q){var
l=a(a9(j),q),m=l[2],d=l[1],g=[0,-1],u=m.length-1,v=0;function
w(h,j){var
k=a(e[r],h[2]),i=9===k[0]?k[2].length-1:0;if(u<i)return j;var
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
c=bH(d);break;default:g[1]=u;var
c=1}else
if(0===f[0])var
c=bK(f[1],d);else{var
m=f[1],t=a(e[125],m),o=b(e[G],d,t);if(o)var
p=o;else{var
l=a(e[r],d);if(16===l[0])var
s=a(n[ch][3],l[1]),q=b(n[17][13],s,m);else
var
q=0;var
p=q}var
c=p}if(c){if(g[1]<i)g[1]=i;return[0,[0,h,i],j]}return j}var
x=h(i[17][16],w,p,v);for(;;){if(0<=g[1]){var
o=g[1];g[1]=-1;var
B=aG(d,o,m),C=function(o,l){return function(u){var
n=u[2],f=u[1];if(o<=n)var
q=o<n?1:0;else
if(5===f[1]){g[1]=o-1|0;var
q=0}else{if(g[1]<n)g[1]=n;var
q=1}if(!q)if(a(a_[2],l))try{var
r=f[1];if(typeof
r==="number")if(2===r){var
v=function(f){var
d=bv(f),g=d[2],c=a(e[36],d[1]),h=c[4],j=c[3],k=c[1],l=b(i[19][39],c[2],g),m=[0,a(e[cc],[0,k,j,h]),l];return a(e[L],m)},C=v(l);a7(k,j,v(f[2]),C);var
p=1}else
if(5<=r){var
x=function(b){return a(e[cc],[0,0,e[117],b])},G=x(l);a7(k,j,x(f[2]),G);var
p=1}else
var
p=0;else
var
p=0;if(!p)a7(k,j,f[2],l);var
D=a(e[L],[0,f[3],f[4]]);try{var
E=X(k,j,D,l)}catch(a){throw z}var
w=aG(d,o,m),F=a(f[7],w);throw[0,av,bJ(w,aF(k,y,E,f[5],F),f)]}catch(b){b=t(b);if(b[1]===av)if(a(A,b[2][1]))throw b;if(b===an){var
B=a(c[1],ds);return h(s[3],0,0,B)}return 0}return 0}}(o,B);b(i[17][11],C,x);continue}bF(f,d);return b(i[19][13],f,m)}}try{var
l=f(d);return l}catch(b){b=t(b);if(b[1]===dt){var
g=a(c[1],du);return h(s[3],0,0,g)}throw b}}var
dw=a4(dv);function
dx(e,d,c,b,a){function
f(a,b){return dr(e,d,c,a,b)}return h(dw[1],f,b,a)}var
dA=a4(dz);function
dB(K,j,g,f,d,c){function
k(k,d){var
r=[0,0],u=[0,0],y=bL(d);function
c(l,f,o,g,p){var
n=a(a9(g),p),k=n[2],d=n[1],j=[0,-1],m=k.length-1,q=0;function
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
4:if(bH(d))var
f=g,c=1;else
var
c=0;break;default:var
f=g,c=1}else
if(0===i[0])if(bK(i[1],d))var
f=g,c=1;else
var
c=0;else
var
l=g+dm(i[1],d,h)|0,n=m<l?-1:l,f=n,c=1;if(!c)var
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
n=a(e[36],d),C=n[4],D=n[3],E=n[2],F=n[1],x=a(e[36],c[3]),G=x[4],H=X(f,g,x[2],E),l=X(b(ao[20],[0,F,D],f),H,G,C),i=1;break;case
5:var
i=0;break;case
3:case
4:var
l=X(f,g,c[3],d),i=1;break;default:var
l=g,i=1}else
if(0===q[0])var
J=a(e[42],c[3])[2],l=bD(f,g,J,0,a(e[42],d)[2]),i=1;else
var
i=0;if(!i)var
I=aG(d,h-(c[4].length-1)|0,k),l=X(f,g,c[3],I);var
z=bD(f,l,c[4],h-(c[4].length-1)|0,k),w=aG(d,h,k),A=a(c[7],w),B=a(K,bJ(w,aF(f,o,z,c[5],A),c));return B}catch(b){b=t(b);if(b[1]===av)if(a(y,b[2][1]))throw b;if(b===ad){r[1]=1;return 0}if(b[1]===dy[1])if(18===b[4][0]){u[1]=1;return 0}if(a(s[22],b))return 0;throw b}}}(x);b(i[17][11],z,w);continue}bF(function(a){return c(l,f,o,g,a)},d);var
A=function(a){return c(l,f,o,g,a)};return b(i[19][13],A,k)}}c(j,g,f,k,d);if(r[1])throw ad;return u[1]}return h(dA[1],k,d,c)}function
a$(b,c){return b[1]?0:(b[1]=[0,a(c,0)],0)}function
ba(d){var
b=d[1];if(b)return b[1];var
e=a(c[1],dC);return h(s[3],0,0,e)}function
dD(d){var
e=d[1];if(e){var
f=e[1],g=f[2],j=f[1];d[1]=[0,[0,j+1|0,g]];try{var
k=b(i[17][5],g,j);return k}catch(a){a=t(a);if(a[1]===dE)throw z;throw a}}var
l=a(c[1],dF);return h(s[3],0,0,l)}function
B(B,y,l,F,x,q){var
d=q[2],I=q[1],u=B?B[1]:0,C=y?y[1]:0,g=[0,0],v=[0,0];if(x){var
k=x[1];if(0===k[1])var
E=k[2],j=0!==E?1:0,f=E;else
var
Q=k[2],j=0===Q?1:0,f=Q}else
var
j=0,f=0;var
n=h(i[17][16],H[5],f,0),K=o.caml_make_vect(n,1-j);function
R(b){var
a=b-1|0;return S(K,a)[a+1]=j}b(i[17][11],R,f);if(0===n)v[1]=j;var
w=[0,0];function
N(b){return a(e[L],[0,b[3],b[4]])}function
D(S){if(l){var
i=l[1],j=i[2],k=i[1];if(d)if(!d[2]){var
w=d[1],x=a(c[6],0),y=A(N(w)),z=a(c[8],4),B=a(c[6],0),C=A(j),D=a(c[1],dI),E=ae(k),F=a(c[1],dJ),G=b(c[13],F,E),H=b(c[13],G,D),I=b(c[13],H,C),J=b(c[13],I,B),K=b(c[13],J,z),L=b(c[13],K,y);return b(c[13],L,x)}var
n=a(c[16],0),o=A(j),p=a(c[1],dG),q=ae(k),r=a(c[1],dH),t=b(c[13],r,q),u=b(c[13],t,p),v=b(c[13],u,o);return b(c[13],v,n)}if(d)if(!d[2]){var
e=d[1],O=a(c[16],0),P=A(N(e)),g=e[1];if(typeof
g==="number")if(5<=g)var
f=0;else
var
m=1-a(bN[38],e[5]),f=1;else
var
f=0;if(!f)var
m=0;var
Q=m?a(c[1],dL):a(c[1],dM),R=b(c[13],Q,P);return b(c[13],R,O)}var
M=a(c[1],dK);return h(s[3],0,0,M)}var
p=[0,0];function
O(a){return p[1]}function
T(a){if(u){p[1]=b(i[18],p[1],[0,a,0]);return 0}throw[0,av,a]}function
P(c){if(c){var
d=c[1],f=d[3],g=d[1],j=c[2],k=f[4],l=f[3],m=b(J[19],g,f[5]),n=b(J[19],g,l),o=a(J[19],g),p=b(i[19][15],o,k),q=function(f){var
c=f[3],d=f[1],l=c[4],o=c[3],q=b(J[19],d,c[5]),r=b(J[19],d,o),s=a(J[19],d),t=b(i[19][15],s,l),g=b(e[G],m,q);if(g)var
j=b(e[G],n,r),k=j?h(dN[31],e[G],p,t):j;else
var
k=g;return 1-k};return[0,d,P(b(i[17][29],q,j))]}return 0}function
U(aa){var
k=w[1];if(k)var
d=a(i[17][3],k[1][2]);else{if(C)throw z;var
$=a(c[1],dU),d=h(s[3],0,0,$)}var
f=d[3],p=d[2],q=d[1],j=a(e[L],[0,f[3],f[4]]);if(n<=g[1])return[0,j,f[6],[0,q,p,f[5]]];if(l)var
m=l[1],r=m[1],t=A(m[2]),u=a(c[1],dO),v=a(c[6],0),x=A(j),y=a(c[8],4),B=a(c[6],0),D=ae(r),E=a(c[1],dP),F=b(c[13],E,D),G=b(c[13],F,B),H=b(c[13],G,y),I=b(c[13],H,x),J=b(c[13],I,v),K=b(c[13],J,u),o=b(c[13],K,t);else
var
X=A(j),Y=a(c[16],0),Z=a(c[1],dT),_=b(c[13],Z,Y),o=b(c[13],_,X);var
N=b(i[15][39],g[1],dQ),O=a(c[1],N),P=a(c[19],n),Q=a(c[1],dR),R=a(c[19],g[1]),S=a(c[1],dS),T=b(c[13],S,R),U=b(c[13],T,Q),V=b(c[13],U,P),W=b(c[13],V,O);return a(M,b(c[13],W,o))}return[0,function(k,y,V,U){a$(w,function(A){var
e=[0,0];try{if(1-u)dx(d,k,F,I,y);e[1]=dB(T,d,k,F,I,y);throw z}catch(d){d=t(d);if(d[1]===av)return[0,0,[0,d[2],0]];var
x=d===z?0:d===ad?0:1;if(!x)if(u)if(0!==O(0))return[0,0,P(O(0))];if(d===z)if(!C){if(e[1]){var
q=a(c[25],dY),r=D(0);return a(M,b(c[13],r,q))}var
v=a(c[1],dZ),w=D(0);return a(M,b(c[13],w,v))}if(d===ad){if(C)throw z;if(l)var
f=l[1][1];else
var
p=a(c[1],dX),f=h(s[3],0,0,p);var
g=ae(de(f)),i=a(c[1],dV),j=D(0),m=a(c[1],dW),n=b(c[13],m,j),o=b(c[13],n,i);return a(M,b(c[13],o,g))}throw d}});if(u)var
E=dD(w);else
var
X=ba(w)[2],E=a(i[17][3],X);var
f=E[3],A=f[4],o=E[1];if(v[1])return y;var
H=f[1];if(typeof
H==="number")switch(H){case
0:var
q=a(e[G],f[3]),p=1;break;case
1:var
q=a(e[G],f[3]),p=1;break;case
2:var
x=a(e[36],f[3]),J=x[4],N=x[2],Q=b(ao[20],[0,x[1],x[3]],k),q=function(d){var
b=a(e[r],d);if(8===b[0]){var
f=b[4],c=at(k,o,N,b[2]);return c?at(Q,o,J,f):c}return 0},p=1;break;case
3:var
q=function(b){return 7===a(e[r],b)[0]?at(k,o,f[3],b):0},p=1;break;default:var
p=0}else
var
p=0;if(!p)var
R=f[3],q=function(a){return at(k,o,R,a)};var
W=A.length-1;function
B(c,l){var
p=c[1],x=c[2];if(v[1])return l;var
r=a(a9(o),l),d=r[2],h=r[1];if(W<=d.length-1)if(a(q,h)){var
w=function(g){var
a=0,e=A.length-1;for(;;){var
b=a===e?1:0;if(b)var
c=b;else{var
f=S(g,a)[a+1],d=at(p,o,S(A,a)[a+1],f);if(d){var
a=a+1|0;continue}var
c=d}return c}};if(b(c9[1],w,d)){var
s=b(i[19][50],A.length-1,d),y=s[2],t=a(e[L],[0,h,s[1]]);g[1]++;if(g[1]===n)v[1]=j;if(g[1]<=n)var
k=g[1]-1|0,u=S(K,k)[k+1];else
var
u=1-j;var
z=u?m(U,p,f[5],t,x):t,C=function(a){return B(c,a)},D=[0,z,b(i[19][56],C,y)];return a(e[L],D)}}function
E(a,c){var
d=c[2],e=c[1],f=0===a[0]?a:[0,a[1],a[3]];return[0,b(ao[20],f,e),d+1|0]}var
F=m(bN[29],E,B,c,h);function
G(a){return B(c,a)}var
H=[0,F,b(i[19][56],G,d)];return a(e[L],H)}return B([0,k,V],y)},U]}function
af(d){switch(d[0]){case
0:return p(d[1]);case
1:var
e=p(d[1]),f=a(c[1],d0);return b(c[13],f,e);case
2:var
g=d[1],h=p(d[2]),i=a(c[1],d1),j=p(g),k=b(c[13],j,i);return b(c[13],k,h);case
3:var
l=d[1],m=p(d[2]),n=a(c[1],d2),o=p(l),q=a(c[1],d3),r=b(c[13],q,o),s=b(c[13],r,n);return b(c[13],s,m);case
4:var
t=d[2],u=d[1],v=p(d[3]),w=a(c[1],d4),x=p(t),y=a(c[1],d5),z=p(u),A=b(c[13],z,y),B=b(c[13],A,x),C=b(c[13],B,w);return b(c[13],C,v);default:var
D=d[2],E=d[1],F=p(d[3]),G=a(c[1],d6),H=p(D),I=a(c[1],d7),J=p(E),K=b(c[13],J,I),L=b(c[13],K,H),M=b(c[13],L,G);return b(c[13],M,F)}}function
bO(d){switch(d[0]){case
0:return p(d[1]);case
1:var
e=p(d[1]),f=a(c[1],d8);return b(c[13],f,e);case
2:var
g=d[1],h=p(d[2]),i=a(c[1],d9),j=a(N[12],g),k=b(c[13],j,i);return b(c[13],k,h);case
3:var
l=d[1],m=p(d[2]),n=a(c[1],d_),o=a(N[12],l),q=a(c[1],d$),r=b(c[13],q,o),s=b(c[13],r,n);return b(c[13],s,m);case
4:var
t=d[2],u=d[1],v=p(d[3]),w=a(c[1],ea),x=a(N[12],t),y=a(c[1],eb),z=p(u),A=b(c[13],z,y),B=b(c[13],A,x),C=b(c[13],B,w);return b(c[13],C,v);default:var
D=d[2],E=d[1],F=p(d[3]),G=a(c[1],ec),H=a(N[12],D),I=a(c[1],ed),J=p(E),K=b(c[13],J,I),L=b(c[13],K,H),M=b(c[13],L,G);return b(c[13],M,F)}}function
em(f){var
d=f[2],g=f[1];function
e(b){var
c=a8(g,g,b);return A(a(i[9],c))}switch(d[0]){case
0:return e(d[1]);case
1:var
h=e(d[1]),j=a(c[1],ee);return b(c[13],j,h);case
2:var
k=d[1],l=e(d[2]),m=a(c[1],ef),n=e(k),o=b(c[13],n,m);return b(c[13],o,l);case
3:var
p=d[1],q=e(d[2]),r=a(c[1],eg),s=e(p),t=a(c[1],eh),u=b(c[13],t,s),v=b(c[13],u,r);return b(c[13],v,q);case
4:var
w=d[2],x=d[1],y=e(d[3]),z=a(c[1],ei),B=e(w),C=a(c[1],ej),D=e(x),E=b(c[13],D,C),F=b(c[13],E,B),G=b(c[13],F,z);return b(c[13],G,y);default:var
H=d[2],I=d[1],J=e(d[3]),K=a(c[1],ek),L=e(H),M=a(c[1],el),N=e(I),O=b(c[13],N,M),P=b(c[13],O,L),Q=b(c[13],P,K);return b(c[13],Q,J)}}function
bb(c,b,a){return af}var
bP=cF(en,af);function
D(e,a){var
c=a[2][2],f=a[1];if(c){var
d=c[1];return[0,f,[0,b(eo[7],e,d)[1],[0,d]]]}return a}function
bc(p,i){ac([$,function(f){var
d=aX(i),e=a(c[1],ep);return b(c[13],e,d)}]);function
e(a){return D(p,v(a))[2]}function
f(e,d,c){var
f=b(H[16],eq,d),g=[0,a(n[69],f)],h=0,i=0===c?V:[4,y,V,c];return[0,e,[0,aq(V,[5,y,g,0,V,i]),h]]}function
k(f,k){if(0===f[0]){var
d=f[1];if(0===d[0])var
b=0;else
var
g=d[1][2],b=1}else
var
b=0;if(!b)var
i=a(c[1],cG),g=h(s[3],0,0,i);var
j=[4,y,[0,[0,[0,[0,y,[0,g]],0],cH,aZ(y)],0],k];return e(aC(y,aZ(y),j))[1]}function
N(b){var
c=1-aY(b);return c?br(a(bz[6],b),er):c}var
O=i[2],P=O[2],d=i[1],W=O[1];if(P){var
g=P[1];if(bq===d)return D(p,[0,40,[0,W,[0,g]]]);if(17===g[0]){var
l=g[2];if(T(l,es))if(T(l,et))if(T(l,eu)){if(!T(l,ev)){var
q=g[3],r=q[1];if(r){var
u=r[2];if(u){var
w=u[2];if(w)if(!w[2])if(!q[2])if(!q[3]){var
Q=u[1],X=w[1],Y=r[1];N(Q);var
Z=[0,k(Q,X),0];return f(d,ew,[0,e(Y)[1],Z])}}}}}else{var
x=g[3],z=x[1];if(z){var
A=z[2];if(A)if(!A[2])if(!x[2])if(!x[3]){var
B=A[1],j=z[1];try{var
R=e(j),m=e(B),C=R[1];if(R[2])if(m[2])var
S=m[1],_=aY(j)?f(d,ey,[0,C,[0,S,[0,k(j,B),0]]]):f(d,ez,[0,C,[0,S,0]]),E=_,o=1;else
var
o=0;else
if(m[2])var
o=0;else
var
E=f(d,eB,[0,C,[0,m[1],0]]),o=1;if(!o)var
aa=a(c[1],eA),E=h(s[3],0,0,aa);return E}catch(a){a=t(a);if(aY(j))return f(d,ex,[0,k(j,B),0]);throw a}}}}else{var
F=g[3],G=F[1];if(G){var
I=G[2];if(I){var
J=I[2];if(J)if(!J[2])if(!F[2])if(!F[3]){var
U=I[1],ab=J[1],ad=G[1];N(U);var
ae=[0,k(U,ab),0];return f(d,eC,[0,e(ad)[1],ae])}}}}else{var
K=g[3],L=K[1];if(L){var
M=L[2];if(M)if(!M[2])if(!K[2])if(!K[3]){var
af=L[1],ag=[0,e(M[1])[1],0];return f(d,eD,[0,e(af)[1],ag])}}}}return D(p,i)}return i}function
bQ(b,a){switch(a[0]){case
0:return[0,bc(b,a[1])];case
1:return[1,D(b,a[1])];case
2:var
c=a[1];return[2,c,D(b,a[2])];case
3:var
d=a[1];return[3,d,D(b,a[2])];case
4:var
e=a[2],f=a[1],g=D(b,a[3]);return[4,D(b,f),e,g];default:var
h=a[2],i=a[1],j=D(b,a[3]);return[5,D(b,i),h,j]}}function
E(c,a){var
d=a[1];return[0,d,b(eE[3],c,a[2])]}function
eF(b,a){switch(a[0]){case
0:return[0,E(b,a[1])];case
1:return[1,E(b,a[1])];case
2:var
c=a[1];return[2,c,E(b,a[2])];case
3:var
d=a[1];return[3,d,E(b,a[2])];case
4:var
e=a[2],f=a[1],g=E(b,a[3]);return[4,E(b,f),e,g];default:var
h=a[2],i=a[1],j=E(b,a[3]);return[5,E(b,i),h,j]}}var
R=a(j[2],eG);function
eH(a,b){return[0,a,bQ(a,b)]}b(C[5],R,eH);b(C[6],R,eF);function
eI(d,c){var
e=a(j[5],bP),f=b(j[7],e,c);return b(ag[9],d,f)}b(q[6],R,eI);var
eJ=a(j[6],bP),eK=[0,a(q[2],eJ)];b(q[3],R,eK);var
eL=a(j[4],R),aw=h(k[13],k[9],eM,eL),eN=0,eO=0;function
eP(a,b){return[0,v(a)]}var
eQ=[0,[0,[0,0,[6,k[15][3]]],eP],eO];function
eR(a,c,b){return[1,v(a)]}var
eS=[6,k[15][3]],eU=[0,[0,[0,[0,0,[0,a(x[12],eT)]],eS],eR],eQ];function
eV(b,e,a,d){var
c=v(b);return[2,v(a),c]}var
eW=[6,k[15][3]],eY=[0,a(x[12],eX)],eZ=[0,[0,[0,[0,[0,0,[6,k[15][3]]],eY],eW],eV],eU];function
e0(b,f,a,e,d){var
c=v(b);return[3,v(a),c]}var
e1=[6,k[15][3]],e3=[0,a(x[12],e2)],e4=[6,k[15][3]],e6=[0,[0,[0,[0,[0,[0,0,[0,a(x[12],e5)]],e4],e3],e1],e0],eZ];function
e7(c,h,b,g,a,f){var
d=v(c),e=v(b);return[4,v(a),e,d]}var
e8=[6,k[15][3]],e_=[0,a(x[12],e9)],e$=[6,k[15][3]],fb=[0,a(x[12],fa)],fc=[0,[0,[0,[0,[0,[0,[0,0,[6,k[15][3]]],fb],e$],e_],e8],e7],e6];function
fd(c,h,b,g,a,f){var
d=v(c),e=v(b);return[5,v(a),e,d]}var
fe=[6,k[15][3]],fg=[0,a(x[12],ff)],fh=[6,k[15][3]],fj=[0,a(x[12],fi)];h(k[23],aw,0,[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,[0,[0,0,[6,k[15][3]]],fj],fh],fg],fe],fd],fc]],eN]]);m(ap[1],R,bb,bb,bb);var
fk=[0,aw,0];function
fl(c){var
d=c[2],e=a(j[4],R);return[0,b(j[7],e,d)]}h(ax[5],fm,fl,fk);function
fn(a){return a[1]}function
fo(a){return a}function
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
b=0;return b?b[1]:br(W(j),fp)}function
aI(p,o,m){var
d=m[2],q=a(l[8],o),c=d[2],e=d[1];if(c){var
f=c[1],g=n[1][9][1],i=p[1],j=function(c,d,a){return b(n[1][9][4],c,a)},k=h(n[1][10][11],j,i,g);return aj(bw[7],1,q,0,0,[0,[0,k,bw[4][2]]],f)}return e}function
az(q,p,o){var
e=fr[10],r=o[2],k=a(j[5],e),l=b(j[7],k,r),c=[0,0],m=b(ag[9],q,l);function
f(b){c[1]=[0,b];return a(ay[13],0)}var
g=b(O[4],m,f),h=a(a(ay[66][8],g),p)[2],d=c[1];if(d){var
i=d[1],n=a(j[6],e);return[0,h,b(ag[2][7],n,i)]}throw[0,P,fq]}function
ah(c,b,a){return aX}function
fs(e){var
f=b(i[23],0,e),c=a(bR[17],f);if(typeof
c!=="number"&&0===c[0]){var
d=c[1];if(!T(d,ft))return 40;if(!T(d,fu))return 64}return 32}var
bS=b(k[1][4][5],fv,fs);function
bT(d,c,b){return[0,a(l[2],c),b]}var
w=a(j[2],fw);function
fx(a,b){return[0,a,bc(a,b)]}b(C[5],w,fx);b(C[6],w,E);function
fy(e,d){var
c=[0,function(f){function
g(a){return bT(e,a,d)}var
c=b(l[48][3],g,f),h=c[2],i=c[1],k=a(j[6],w),m=a(q[2],k),n=b(q[1][8],m,h),o=[0,a(O[1],n),i];return a(bd[21][5],o)}];return a(O[8],c)}b(q[6],w,fy);b(q[3],w,0);var
fz=a(j[4],w),aJ=h(k[13],k[9],fA,fz),fB=0,fC=0;function
fD(a,c,b){return v(a)}var
fE=[6,k[15][1]],fG=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(x[12],fF)]],fE],fD],fC]],fB]];h(k[23],aJ,0,fG);m(ap[1],w,ah,ah,ah);var
fH=[0,aJ,0];function
fI(c){var
d=c[2],e=a(j[4],w);return[0,b(j[7],e,d)]}h(ax[5],fJ,fI,fH);var
bU=bR[4],fK=0,fL=0;function
fM(c,b,e){var
d=ar(b,c),f=a(bU,e);if(bg(W(d),f))if(40===b)return ar(bq,c);return d}h(k[1][6],aJ,0,[0,[0,0,0,[0,[0,[0,[2,bS],[0,[2,k[15][1]],0]],fM],fL]],fK]);var
K=a(j[2],fN);function
fO(a,b){return[0,a,bc(a,b)]}b(C[5],K,fO);b(C[6],K,E);function
fP(e,d){var
c=[0,function(f){function
g(a){return bT(e,a,d)}var
c=b(l[48][3],g,f),h=c[2],i=c[1],k=a(j[6],K),m=a(q[2],k),n=b(q[1][8],m,h),o=[0,a(O[1],n),i];return a(bd[21][5],o)}];return a(O[8],c)}b(q[6],K,fP);var
fQ=a(j[6],w),fR=[0,a(q[2],fQ)];b(q[3],K,fR);var
fS=a(j[4],K),aK=h(k[13],k[9],fT,fS),fU=0,fV=0;function
fW(a,c,b){return v(a)}var
fX=[6,k[15][3]],fZ=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(x[12],fY)]],fX],fW],fV]],fU]];h(k[23],aK,0,fZ);m(ap[1],K,ah,ah,ah);var
f0=[0,aK,0];function
f1(c){var
d=c[2],e=a(j[4],K);return[0,b(j[7],e,d)]}h(ax[5],f2,f1,f0);var
f3=0,f4=0;function
f5(c,b,e){var
d=ar(b,c),f=a(bU,e);if(bg(W(d),f))if(40===b)return ar(bq,c);return d}h(k[1][6],aK,0,[0,[0,0,0,[0,[0,[0,[2,bS],[0,[2,k[15][3]],0]],f5],f4]],f3]);function
f6(l,d,c){var
o=a(n[1][9][5],l),h=b(aA[4][1],d,c),p=b(aA[4][4],d,c),i=[0,a(g[98],d)];try{var
u=a(ao[10],h),v=[0,b5(Y[53],h,i,u,p,o)],e=v}catch(a){a=t(a);if(a[1]!==Y[51])throw a;var
e=0}if(e){var
j=e[1],k=i[1],q=j[2],r=j[1],s=b(aA[4][5],k,c),f=m(aA[4][6],k,r,q,s);return m(aA[4][8],f[3],c,f[1],f[2])}return d}function
bV(F,m,k,E,D){ac([$,function(f){var
d=af(E),e=a(c[1],f8);return b(c[13],e,d)}]);ac([$,function(i){var
d=a(n[1][10][17],m[1]);function
e(d){var
e=d[1],f=a(q[1][4],d[2][1]),g=a(c[1],f7),h=a(N[12],e),i=b(c[13],h,g);return b(c[13],i,f)}var
f=h(c[53],c[16],e,d),g=a(c[1],f9);return b(c[13],g,f)}]);function
B(b,a){return[2,b,a]}function
al(b,a){return[3,b,a]}function
am(a){return[1,a]}function
p(a,b){var
c=a?a[1]:32;return[0,c,[0,b,0]]}function
w(h,g,p,u,o){try{var
e=aI(h,k,g);switch(e[0]){case
1:var
q=e[1][2];if(b(n[1][10][3],q,h[1]))if(a(ai[3],p))var
f=1;else
if(a(ai[3],F))var
f=1;else
var
v=b(n[1][10][22],q,h[1]),w=a(ai[7],F),x=a(j[6],w),y=b(ag[2][7],x,v),i=b(ai[7],p,y),c=1,f=0;else
var
f=1;if(f)var
c=0;break;case
14:if(13===e[2][0]){var
l=e[3];if(typeof
l==="number")var
d=1;else
if(0===l[0]){var
m=l[1];if(5===m[0]){var
r=m[2];if(r)var
i=b(u,r[1],[0,32,[0,m[5],0]]),c=1,d=0;else
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
i=a(o,g);return i}catch(b){b=t(b);if(a(s[22],b))return a(o,g);throw b}}function
u(c,b,a){return w(m,p(0,c),0,b,a)}function
C(d,i){var
e=a(c[1],d),f=a(c[1],f_),g=b(c[13],f,e);return h(s[3],0,0,g)}function
G(u,j,s,c){var
m=a(e[r],u);if(3===m[0]){var
v=m[1][1],n=a(l[9],k),o=a(au[2][4],n),d=[0,0];try{b(au[2][5],j,n);var
z=function(m){var
e=0===d[1]?1:0;if(e){var
n=b(g[23],c,m),f=a(g[6],n),h=a(au[2][4],f),j=o<h?1:0;if(j){var
p=b(i[17][5],f,(h-o|0)-1|0);d[1]=[0,a(au[2][1][1],p)];var
k=0}else
var
k=j;var
l=k}else
var
l=e;return l},f=d,p=z}catch(a){a=t(a);if(a!==an)throw a;var
f=[0,[0,j]],p=function(a){return 0}}var
w=a(l[2],k),q=function(d,f){var
j=a(e[r],f);if(3===j[0]){var
c=j[1][1];if(!bh(c,v))if(!b(i[17][26],c,d))if(!b(g[26],w,c)){p(c);return[0,c,d]}return d}return h(e[142],q,d,f)},x=q(0,b(Y[32],c,s)),y=function(c,d){if(b(g[34],c,d))return c;if(a(ai[3],f[1]))return c;var
e=a(ai[7],f[1]);ac([$,function(b){return a(N[12],e)}]);return f6(e,c,d)};return h(i[17][15],y,c,x)}throw[0,P,f$]}function
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
e=k[5],A=z[1],D=a(n[68],A),E=8<b7(D)?1:0,J=E?o.caml_string_equal(h(i[15][4],D,0,8),ga):E;if(J){var
c=a(n[68],A),F=h(i[15][4],c,8,b7(c)-8|0);if(T(F,gb)){if(!T(F,gc))if(4===e[0]){var
l=e[3];if(l){var
m=l[2],q=l[1];if(!m)return u(q,B,function(a){return[0,a]});var
r=m[2],G=m[1];if(!r){var
N=function(a){return C(c,a)},O=p(0,q);return u(G,function(a,b){return[4,O,a,b]},N)}if(!r[2]){var
K=r[1],L=function(a){return u(K,B,function(a){throw[0,P,gd]})},M=p(0,q);return u(G,function(a,b){return[4,M,a,b]},L)}}}}else
if(4===e[0]){var
s=e[3];if(s){var
t=s[2];if(t)if(!t[2]){var
Q=t[1],R=s[1],S=function(a){return C(c,a)},U=p(0,R);return u(Q,function(a,b){return[5,U,a,b]},S)}}}return C(c,0)}var
d=1}else
var
d=1}else
var
d=1}else
var
d=0}return w(f,x,[0,H],B,function(a){return[0,a]});case
1:return w(f,b[1],0,al,am);case
2:var
I=b[1],V=b[2],W=function(a){return[2,aH(I),a]};return w(f,V,0,function(a,b){return[4,I,a,b]},W);case
3:var
X=b[2];return[3,aH(b[1]),X];case
4:var
Y=b[3],Z=b[1];return[4,Z,aH(b[2]),Y];default:var
_=b[3],$=b[1];return[5,$,aH(b[2]),_]}}var
d=H([0,m,E]);ac([$,function(g){var
e=bO(d),f=a(c[1],ge);return b(c[13],f,e)}]);if(D){var
x=[0,32,D[1]];switch(d[0]){case
0:var
I=d[1],ao=W(I),v=[0,a0(I,x,function(a,b){return aC(ao,a,b)},aq)];break;case
2:var
aB=d[2],aD=d[1],v=[5,p(0,aq(V,aI(m,k,x))),aD,aB];break;case
4:var
aj=d[3],aE=d[2],aF=d[1],aG=p(0,aI(m,k,x)),aJ=W(aj),v=[4,a0(aF,aG,function(a,b){return aC(aJ,a,b)},aq),aE,aj];break;case
5:var
ak=d[3],aK=d[2],aL=d[1],aM=p(0,aI(m,k,x)),aN=W(ak),v=[5,a0(aL,aM,function(a,b){return aC(aN,a,b)},aq),aK,ak];break;default:var
v=d}var
f=v}else
var
f=d;ac([$,function(g){var
d=bO(f),e=a(c[1],gf);return b(c[13],e,d)}]);function
J(a,b,c){var
d=c[2],e=d[2],f=d[1],g=c[1];if(e){var
h=e[1];return[0,g,[0,f,[0,[5,a,[0,a,b],aZ(a),h]]]]}return[0,g,[0,[7,a,b,[13,[0,a,[1,b],0,0]],f],0]]}switch(f[0]){case
0:var
K=az(m,k,f[1]);return[0,K[1],[0,K[2]]];case
1:var
L=az(m,k,f[1]);return[0,L[1],[1,L[2]]];case
2:case
3:var
M=f[1],O=az(m,k,J(y,[0,M],f[2])),ap=O[1],Q=a(e[36],O[2]),R=Q[4],z=Q[2],S=G(z,M,R,ap),ar=b(Y[32],S,R),U=b(a_[13],z,ar),as=2===f[0]?[2,z,U]:[3,z,U];return[0,S,as];default:var
X=f[2],at=f[1],Z=az(m,k,J(y,[0,X],f[3])),av=Z[1],_=a(e[36],Z[2]),aa=_[4],A=_[2],ab=G(A,X,aa,av),aw=b(Y[32],ab,aa),ad=b(a_[13],A,aw),ax=a(g[68],k),ae=az(m,b(l[3],ax,ab),at),ah=ae[2],ay=ae[1],aA=4===f[0]?[4,ah,A,ad]:[5,ah,A,ad];return[0,ay,aA]}}function
bW(d,c,b,a){return bV(0,d,c,[0,b],a)}function
gg(d){var
b=d[2];if(0===b[0]){var
c=a(e[r],b[1]);return 1===c[0]?[0,c[1]]:0}return 0}function
bX(v,i,j,o,n,q,p){function
f(c,a){return b(J[19],c,a)}function
w(f,e,k){var
d=b(g[23],f,e),i=d[3];if(i)var
j=i[1];else
var
n=a(c[1],gh),o=a(c[1],gi),p=a(c[16],0),q=a(bM[1],e),r=a(c[19],q),s=a(c[1],gj),t=a(aB,k),u=a(c[1],gk),v=b(c[13],u,t),w=b(c[13],v,s),x=b(c[13],w,r),y=b(c[13],x,p),z=b(c[13],y,o),j=a(M,b(c[13],z,n));var
l=[0,d[1],d[2],0,d[4],d[5],d[6],d[7]],m=b(g[25],f,e);return[0,h(g[22],m,e,l),j]}function
x(c){var
b=a(e[r],c);if(3===b[0])return b[1][1];throw[0,P,gl]}function
l(j,i,h,b,a,g){var
c=b[2],d=b[1],k=a?a[1]:c,e=bI(0,j,i,h,[0,d,k],g,0,f(d,c));return[0,e[1],[0,e[2],0]]}if(n){var
D=n[1],k=D[2],d=D[1];switch(k[0]){case
4:var
N=k[2],ag=k[3],ah=f(d,k[1]),t=f(d,ag),ai=x(N),O=B(0,0,0,j,_,l(gn,i,j,[0,d,t],0,I)),aj=O[2],ak=O[1],Q=B(0,0,0,d,_,l(0,i,d,[0,d,N],0,I)),al=Q[2],am=Q[1],R=B(0,v,0,j,q,l(0,i,j,[0,d,ah],0,I)),an=R[2],ao=R[1],ap=m(ak,i,o,1,function(b,i,n,h){var
c=Z(b,a(g[ab],d),i,t),e=w(c,ai,t),j=e[2],k=e[1];function
l(b,d,c,a){return m(ao,b,j,a,p)}return f(c,m(am,b,f(k,t),h,l))});a(an,0);a(al,0);a(aj,0);return ap;case
5:var
y=k[2],aq=k[3],z=f(d,k[1]),u=f(d,aq),ar=x(y),S=Z(i,d,y,z),T=B(0,0,0,j,_,l(go,i,j,[0,S,f(S,u)],0,I)),as=T[2],at=T[1],U=B(0,0,0,d,q,l(0,i,d,[0,d,y],0,I)),au=U[2],av=U[1],aw=m(at,i,o,1,function(b,j,n,i){var
c=Z(b,a(g[ab],d),j,u),e=w(c,ar,u),h=e[1],k=e[2];function
l(a,e,d,c){var
b=f(Z(a,h,k,z),z);return m(p,a,b,b,c)}return f(c,m(av,b,f(h,u),i,l))});a(au,0);a(as,0);return aw;case
0:case
1:var
V=f(d,k[1]),W=a(g[ab],d);if(n)if(0===n[1][2][0])var
E=q,A=1;else
var
A=0;else
var
A=0;if(!A)var
E=_;var
F=B(0,v,0,j,E,l(0,i,j,[0,W,V],0,I)),X=F[2],Y=m(F[1],i,o,1,p);a(X,0);return Y;default:var
G=k[1],s=f(d,k[2]);if(n)if(2===n[1][2][0])var
H=q,C=1;else
var
C=0;else
var
C=0;if(!C)var
H=_;var
$=x(G),K=B(0,0,0,j,_,l(gm,i,j,[0,d,s],0,I)),aa=K[2],ac=K[1],L=B(0,v,0,d,H,l(0,i,d,[0,d,G],0,I)),ad=L[2],ae=L[1],af=m(ac,i,o,1,function(c,j,o,i){var
e=Z(c,a(g[ab],d),j,s),h=w(e,$,s),k=h[2],l=h[1];function
n(a,c){return b(p,a,k)}return f(e,m(ae,c,f(l,s),i,n))});a(ad,0);a(aa,0);return af}}return m(p,i,o,o,1)}function
bY(e,l,d){var
f=d[2],i=d[1],m=e?e[1]:0;switch(f[0]){case
1:case
3:var
o=a(c[1],gq),j=h(s[3],0,0,o);break;default:var
j=f[1]}var
k=m?b6(bE[29],0,0,0,0,gp,l,i):i,n=a(g[cj],k);return[0,b(J[19],k,j),n]}function
bZ(m,f,l,k,d,c,j){if(bh(c,gr))var
i=0,h=gs;else
var
i=1,h=c;var
b=[0,0],n=bX(m,f,l,k,[0,d],h,function(h,c,f,d){a$(b,function(a){return[0,c,g[b$]]});return i?a(e[aO],(d+j|0)-1|0):c}),o=0===b[1]?bY(0,f,d):ba(b);return[0,o,n]}function
be(g,f,e,d,c,b,a){return bI(g,0,f,e,d,c,b,a)}function
gt(f,p,o,d,n,c,l,k){var
q=c[2],h=be(0,f,d,[0,a(g[ab],c[1]),q],l,0,n),i=B(0,gu,0,d,o,[0,h[1],[0,h[2],0]]),r=i[2],s=i[1],t=m(s,f,p,k,function(c,b,a){return e[aO]}),j=a(r,0),b=j[3];return[0,b[1],b[2],b[3],t,j[1]]}function
gv(k,j,o,d,i){var
e=i[2],l=i[1];try{var
f=gt(k,j,o,d,e,[0,l,e],I,1),n=f[1],B=f[4],C=f[3],D=f[2],E=n!==d?a(s[6],gy):[0,B,[0,b(g[bi],n,D),C]];return E}catch(f){f=t(f);if(f===z)try{var
v=function(a){return 1},h=aF(k,d,a(g[ab],l),e,v),m=h[1],w=h[3],x=h[2];if(m!==d)throw z;var
y=[0,j,[0,b(g[bi],m,x),w]];return y}catch(d){var
p=a(c[1],gw),q=A(e),r=a(c[1],gx),u=b(c[13],r,q);return a(M,b(c[13],u,p))}throw f}}function
gz(b,e,d){var
f=a(l[2],b),g=a(l[8],b),c=gv(g,a(l[7],b),e,f,d);return[0,c[1],c[2][2]]}function
gA(a){return[0,32,[0,[0,[0,y,[0,a],0]],0]]}function
gB(c){var
a=c[2],b=a[2],d=a[1],e=b?12===b[1][0]?1:0:13===d[0]?1:0;return e?1:0}function
gC(d,c,b,a){return af(a[2])}function
b0(d,c,b,a){return af(a)}var
F=a(j[2],gD);function
gE(a,b){return[0,a,bQ(a,b)]}b(C[5],F,gE);function
gF(b,a){return a}b(C[6],F,gF);function
gG(e,d){var
c=[0,function(f){function
g(b){return[0,a(l[2],b),[0,e,d]]}var
c=b(l[48][3],g,f),h=c[2],i=c[1],k=a(j[6],F),m=a(q[2],k),n=b(q[1][8],m,h),o=[0,a(O[1],n),i];return a(bd[21][5],o)}];return a(O[8],c)}b(q[6],F,gG);b(q[3],F,0);b(k[11],F,aw);m(ap[1],F,b0,b0,gC);var
gH=[0,aw,0];function
gI(c){var
d=c[2],e=a(j[4],F);return[0,b(j[7],e,d)]}h(ax[5],gJ,gI,gH);function
gK(d,c){var
e=a(l[2],c),f=b(g[bi],e,d),h=a(g[68],c);return b(l[3],h,f)}function
gL(d,c){var
e=a(l[2],c),f=b(g[bj],e,d),h=a(g[68],c);return b(l[3],h,f)}function
b1(c,b,a){return bV([0,F],c,b,a,0)}var
gP=[0,function(p,c){var
d=c[1],f=a(n[1][5],gO),i=b(n[1][10][22],f,d),m=a(j[6],F),k=b(ag[2][7],m,i);function
o(c){var
q=b1(k[1],c,k[2]),r=a(l[2],c),s=a(l[7],c),f=bZ(0,a(aW[2],0),r,s,q,_,1),i=f[1][1],t=f[2],d=b(l[16],c,i),j=d[2],m=d[1],o=a(g[68],c),p=b(l[3],o,m),u=[0,[0,a(n[69],gM)],i,j,t],v=a(e[cr],u),w=h(gN[3],0,v,2);return b(ay[66][8],w,p)}return a(ay[66][1],o)}];h(bf[9],0,b2,gP);var
gQ=[31,aS[4],[0,b2,0],0],gS=[28,[0,[0,[0,a(n[1][5],gR)],0],gQ]];function
gT(c){var
b=a(n[1][5],gU);return m(bf[4],1,0,b,gS)}b(aR[19],gT,gV);function
gW(n,k,d){var
o=a(l[7],d),e=a(l[2],d),f=a(l[8],d),g=bW(n,d,k,0),h=g[2],p=g[1],i=0===h[0]?h[1]:a(M,a(c[1],g4)),q=0,j=be(0,f,e,[0,p,i],function(a,b){return 1},q,i),r=B(gY,gX,0,e,0,[0,j[1],[0,j[2],0]])[1];function
s(t,e,d,s){var
f=a(aB,d),g=a(c[16],0),h=a(c[1],gZ),i=a(c[16],0),j=a(aB,e),k=a(c[16],0),l=a(c[1],g0),m=b(c[13],l,k),n=b(c[13],m,j),o=b(c[13],n,i),p=b(c[13],o,h),q=b(c[13],p,g),r=b(c[13],q,f);b(aT,0,b(c[29],1,r));return d}b(aT,0,a(c[1],g1));try{for(;;){m(r,f,o,1,s);continue}}catch(e){e=t(e);if(e===z){b(aT,0,a(c[1],g2));return a(g3[1],d)}throw e}}var
g5=0,g7=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(j[6],w),f=b(ag[2][7],e,d);return function(b){function
c(a){return gW(b,f,a)}return a(ay[66][1],c)}}return a(H[2],g6)},g5],g8=a(i[19][12],g7);h(bf[9],0,[0,aQ,g9],g8);function
g_(f){var
c=0,d=0,e=a(n[1][6],g$);if(0===w[0])return b(ax[4],[0,aQ,hc],[0,[0,hb,[0,[1,aS[4],[5,[0,w[1]]],e],d]],c]);throw[0,P,ha]}b(aR[19],g_,aQ);a(x[9],cs);var
b3=[0,aX,aJ,w,aK,K,af,aw,R,z,ad,em,bY,b1,bW,bX,bZ,ae,be,B,gz,fo,a$,ba,Z,da,fn,W,gg,gB,gA,A,gK,gL,bt,bA];b8(251,b3,"Ssrmatching_plugin.Ssrmatching");b8(252,[0,b3],"Ssrmatching_plugin");return});

function(j$){"use strict";var
bd=104,cz=",",K='"',cy="field_mod",r=250,cx="(",cw="constants",aM="Field_tac",ce="with carrier ",cu="postprocess tactic",cv="Init",aL="field",ct="InitialRing",m=246,cd="decidable",cq="Pphi_pow",cr='Using setoid "',cs="tactic recognizing constants",cp="l",co="postprocess",bc="gen_phiZ",aN="setoid",cn="ring_mod",C="]",cc="preprocess",a_="power_tac",cm="Print",bb="power",cl="Ring_polynom",ba="protect_fv",ak="Ring",cb='and "',ca=103,b$="morphism",a9="plugins/setoid_ring/newring.ml",ck="field_lookup",cj=107,$="Coq",b9="lH",b_="closed",a$="x",b8="ring_lookup",b7="f",a8="ring",b5="map",b6="preprocess tactic",B="[",ci='and morphisms "',b4="and equivalence relation ",b3=")",b1="field_mods",b2=":",aK="t",b0="abstract",aJ="sign",bZ="PEeval",aI="div",a7="newring_plugin",cg="Add",ch="completeness",a5="id",a6="IDphi",cf="ring_mods",bX=129,bY="Pphi_dev",J=j$.jsoo_runtime,G=J.caml_check_bound,b=J.caml_new_string,q=J.caml_obj_tag,aH=J.caml_register_global,A=J.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):J.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):J.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):J.caml_call_gen(a,[b,c,d])}function
P(a,b,c,d,e){return a.length==4?a(b,c,d,e):J.caml_call_gen(a,[b,c,d,e])}function
F(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):J.caml_call_gen(a,[b,c,d,e,f])}function
j_(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):J.caml_call_gen(a,[b,c,d,e,f,g,h])}var
f=J.caml_get_global_data(),bi=[0,b(a7),b("closed_term")],bp=[0,b(a7),b("get_res")],br=[0,[0,b($),[0,b("Setoids"),[0,b("Setoid"),0]]],[0,[0,b($),[0,b("Lists"),[0,b("List"),0]]],[0,[0,b($),[0,b(cv),[0,b("Datatypes"),0]]],[0,[0,b($),[0,b(cv),[0,b("Logic"),0]]],0]]]],an=b("setoid_ring"),aj=b(a7),X=f.Constr,h=f.Util,Q=f.CClosure,ae=f.Termops,al=f.Globnames,H=f.Not_found,i=f.EConstr,bO=f.Tacmach,s=f.Proofview,g=f.Ltac_plugin,O=f.Coqlib,aa=f.Global,l=f.Names,x=f.Stdlib,bK=f.Lib,p=f.CamlinternalLazy,aU=f.UnivGen,y=f.Mod_subst,U=f.Libnames,_=f.Retyping,bx=f.Reductionops,d=f.Pp,W=f.CErrors,D=f.Printer,aS=f.Assert_failure,aw=f.Typing,aT=f.Option,Y=f.Evarutil,bE=f.Smartlocate,I=f.Loc,ad=f.Feedback,bA=f.Flags,v=f.Stdarg,j=f.Genarg,af=f.Evd,bm=f.Univops,bn=f.Declare,bl=f.Constrintern,bh=f.Tactics,at=f.Summary,au=f.Libobject,aF=f.Vernac_classifier,bS=f.Pfedit,E=f.Ppconstr,n=f.Pcoq,k=f.CLexer,bT=f.Vernacentries,be=[0],cE=f.Esubst,c1=f.CAst,c3=f.Refiner,cU=f.Environ,cV=f.Goal,cQ=f.DAst,cM=f.Tacticals,fL=f.Redexpr,gD=f.Mltop;aH(272,be,"Newring_plugin.Newring_ast");var
gB=[0,0,0],gC=[0,0,0],gq=b("field kind"),gr=b(cs),gs=b(b6),gt=b(cu),gu=b(aN),gv=b(bb),gw=b(aJ),gx=b(aI),gy=b("infinite property"),gg=[0,b(aM),0],gh=[0,0,0],gi=b("field_lemmas"),gj=b("_field_lemma1"),gk=b("_field_lemma2"),gl=b("_field_lemma3"),gm=b("_field_lemma4"),gn=b("_lemma5"),gp=[22,0],go=[22,0],gf=b("field inverse should be declared as a morphism"),f2=b("arguments of field_simplify do not have all the same type"),f3=[0,b(aL)],f1=[0,b(aM),0],f4=b(K),f5=b(K),f6=b("cannot find a declared field structure over"),f7=[0,b(aL)],f8=[0,b(a9),856,12],fW=[0,1],fX=[0,0],fV=b("bad field structure"),eY=[0,0,0],eZ=[0,0,0],eO=b("ring kind"),eP=b(cs),eQ=b(b6),eR=b(cu),eS=b(aN),eT=b(bb),eU=b(aJ),eV=b(aI),eN=b(" cannot be set twice"),eH=[0,b("Ring_base"),0],eI=b("ring_lemmas"),eJ=b("_ring_lemma1"),eK=b("_ring_lemma2"),eM=[22,0],eL=[22,0],eF=[0,1],eG=[0,0],eE=b("bad ring structure"),en=b("ring addition should be declared as a morphism"),eo=b("ring multiplication should be declared as a morphism"),ep=b("ring opposite should be declared as a morphism"),eq=b(K),er=b(cb),es=b(K),et=b(K),eu=b('",'),ev=b(ci),ew=b(K),ex=b(cr),ey=b(K),ez=b(cb),eA=b(K),eB=b(ci),eC=b(K),eD=b(cr),ej=b("cannot find setoid relation"),d7=b("arguments of ring_simplify do not have all the same type"),d8=[0,b(a8)],d9=b(K),d_=b(K),d$=b("cannot find a declared ring structure over"),ea=[0,b(a8)],eb=[0,b(a9),354,12],dn=[0,b(cl),0],dm=b("newring"),dh=b(ak),dg=b(ak),dc=b("ring: cannot find relation (not closed)"),db=b("ring: cannot find relation"),c5=b(ak),c4=b(ak),c2=b(a$),c0=b(a$),cY=b(a$),cW=b("Ring.exec_tactic: anomaly"),cS=[2,1],cT=[0,1],cP=b(aK),cR=b(aK),cL=[0,b(a9),116,7],cG=b("not found"),cH=b("map "),cI=[0,b("lookup_map")],cD=b("dummy"),cB=b("global_head_of_constr."),c6=b("Build_Setoid_Theory"),c8=b("None"),c9=b("Some"),c_=b("eq"),c$=b("cons"),da=b("nil"),dd=b($),de=[0,[0,b("Ring_theory"),0],[0,[0,b(cl),0],[0,[0,b("Ring_tac"),0],[0,[0,b(ct),0],[0,[0,b(aM),0],[0,[0,b("Field_theory"),0],0]]]]]],di=[0,b($),0],dj=b(ct),dp=b("almost_ring_theory"),dq=b("Eqsth"),ds=b("Eq_ext"),du=b("Eq_s_ext"),dw=b("ring_theory"),dx=b("mk_reqe"),dz=b("semi_ring_theory"),dA=b("mk_seqe"),dC=b("Abstract"),dD=b("Computational"),dF=b("Morphism"),dH=b("inv_morph_nothing"),dI=b("mkhypo"),dK=b("hypo"),dN=b(bZ),dQ=b(cq),dT=b(bY),dW=b(bc),dZ=b(a6),d3=b(a8),d4=b("ring-tac-carrier-table"),d5=b("ring-tac-name-table"),ed=b("tactic-new-ring-theory"),e1=[0,b($),0],e2=b(aM),e7=b("FEeval"),e_=b(bZ),fb=b(cq),fe=b(bY),fh=b("display_pow_linear"),fk=b("display_linear"),fn=b(bc),fq=b(a6),fu=b(aL),fx=b("PCond"),fA=b(bc),fD=b(a6),fH=b("field_cond"),fI=b(aL),fK=b("simpl_field_expr"),fM=b("almost_field_theory"),fN=b("field_theory"),fO=b("semi_field_theory"),fP=b("AF_AR"),fR=b("F_R"),fT=b("SF_SR"),fY=b("field-tac-carrier-table"),fZ=b("field-tac-name-table"),f_=b("tactic-new-field-theory"),jG=b(b4),jH=b(ce),jF=b("The following field structures have been declared:"),je=b(ch),iM=b(b4),iN=b(ce),iL=b("The following ring structures have been declared:"),hn=[0,0],gR=b(b0),gS=b(cd),gT=b(b$),gU=b(C),gV=b(B),gW=b(cw),gX=b(C),gY=b(B),gZ=b(b_),g0=b(C),g1=b(B),g2=b(cc),g3=b(C),g4=b(B),g5=b(co),g6=b(aN),g7=b(C),g8=b(B),g9=b(a_),g_=b(C),g$=b(B),ha=b(a_),hb=b(aJ),hc=b(aI),gG=b(b5),gH=b(ba),gK=b(a5),gL=b("in"),gN=b(b5),gO=b(ba),gQ=b(ba),hd=b(cn),hf=b(cn),hk=b(cd),ho=b(b0),hs=b(b$),hv=b(C),hy=b(B),hA=b(cw),hD=b(C),hG=b(B),hI=b(b_),hL=b(C),hO=b(B),hQ=b(cc),hT=b(C),hW=b(B),hY=b(co),h3=b(aN),h7=b(aJ),h_=b(C),ib=b(B),ie=b(bb),ii=b(C),il=b(B),ip=b(a_),it=b(aI),iw=b(cf),iy=b(cf),iC=b(b3),iE=b(cz),iG=b(cx),iO=[0,b(cm),[0,b("Rings"),0]],iS=[0,b(cp)],iU=[0,b(aK)],iV=b(b2),iX=[0,b(a5)],iY=b(ak),iZ=b(cg),i3=b("AddSetoidRing"),i6=b("lrt"),i7=b(C),i9=b(b9),i_=b(B),ja=b(b7),jb=b(b8),jd=b(b8),jf=b(cy),jh=b(cy),jn=b(ch),jq=b(b1),js=b(b1),jw=b(b3),jy=b(cz),jA=b(cx),jI=[0,b(cm),[0,b("Fields"),0]],jM=[0,b(cp)],jO=[0,b(aK)],jP=b(b2),jR=[0,b(a5)],jS=b("Field"),jT=b(cg),jX=b("AddSetoidField"),j0=b("lt"),j1=b(C),j3=b(b9),j4=b(B),j6=b(b7),j7=b(ck),j9=b(ck);function
L(b){var
c=a(d[3],b);return e(W[6],0,0,c)}function
cA(b,f){var
g=c(i[83],b,f)[1];try{var
j=c(ae[ca],b,g)[1];return j}catch(b){b=A(b);if(b===H){var
h=a(d[3],cB);return e(W[3],0,0,h)}throw b}}function
cC(b){try{var
c=a(al[16],b);return c}catch(b){b=A(b);if(b===H)return[0,a(l[1][6],cD)];throw b}}function
bf(g,f,e){var
i=a(X[65],e),j=i[2],b=i[1],k=a(g,cC(b));if(k){var
l=k[1],d=function(d,b){switch(a(l,d)){case
0:var
e=a(cE[1],f);return c(Q[55],e,b);case
1:return a(Q[38],b);default:return bf(g,f,b)}};if(a(h[19][31],j))return d(-1,b);var
m=c(h[19][16],d,j),n=[6,d(-1,b),m];return a(Q[39],n)}return a(Q[38],e)}function
bg(b,a){try{var
c=[0,e(h[17][119],l[68][1],a,b)];return c}catch(a){a=A(a);if(a===H)return 0;throw a}}var
aO=[0,h[15][52][1]];function
aP(b,a){aO[1]=e(h[15][52][4],b,a,aO[1]);return 0}function
cF(f){try{var
b=c(h[15][52][22],f,aO[1]);return b}catch(b){b=A(b);if(b===H){var
g=a(d[3],cG),i=a(d[20],f),j=a(d[3],cH),k=c(d[12],j,i),l=c(d[12],k,g);return e(W[6],0,cI,l)}throw b}}function
aQ(h,g,f,b){function
j(a){return c(Y[12],f,a)}var
k=a(i[bX][1],b),l=a(Q[22],0),m=e(Q[43],[0,j],Q[8],g),n=c(cF(h),f,b);function
d(c,f){var
b=a(X[26],f);if(6===b[0]){var
h=b[2],i=b[1],j=d(c+1|0,b[3]),k=[0,i,d(c,h),j];return a(X[10],k)}var
g=bf(n,c,f);return e(Q[59],m,l,g)}var
o=d(0,k);return a(i[8],o)}function
cJ(a){var
b=0,c=2,d=[0,function(b,c,d){return aQ(a,b,c,d)},c];return e(bh[49],0,d,b)}function
cK(b,a){var
c=[0,[0,a,0]],d=2,f=[0,function(a,c,d){return aQ(b,a,c,d)},d];return e(bh[49],0,f,c)}function
aR(a,e,b){var
f=b;for(;;)try{var
l=c(ae[ca],a,f)[1],m=c(al[21][3],l,e);return m}catch(b){b=A(b);if(b===H){var
d=c(i[3],a,f);switch(d[0]){case
5:var
f=d[1];continue;case
9:var
j=d[2],g=aR(a,e,d[1]);if(g){var
k=function(b){return aR(a,e,b)};return c(h[19][34],k,j)}return g;default:return 0}}throw b}}var
cN=[0,function(b,r){if(b){var
f=b[2];if(f)if(!f[2]){var
i=f[1],k=a(g[13][2][2],b[1]),l=a(aT[7],k),m=a(g[13][2][5],i),n=a(aT[7],m),o=function(b){var
d=a(j[6],v[9]);return c(g[13][2][7],d,b)},p=c(h[17][69],o,n),q=function(b){if(aR(b,e(h[17][16],al[21][4],p,al[21][1]),l))return a(s[16],0);var
f=a(d[7],0);return c(cM[66][4],0,f)};return c(s[72][1],s[55],q)}}throw[0,aS,cL]}];e(g[4][16],0,bi,cN);var
cO=[0,bi,0];function
bj(b){function
d(a){return[0,c(I[11],0,a)]}var
e=c(h[17][69],d,b),f=a(j[18],v[9]),g=a(j[5],f),i=[0,[0,c(j[7],g,e)],0],k=[1,a(l[1][6],cP)],m=[0,c(cQ[3],0,k),0],n=a(j[5],v[11]),o=[0,cO,[0,[0,c(j[7],n,m)],i]],p=[31,c(I[11],0,o)];return[28,[0,[0,[0,a(l[1][6],cR)],0],p]]}function
bk(d){var
b=a(aa[2],0),f=a(af[17],b),c=e(bl[13],b,f,d);return[0,c[1],c[2]]}function
M(c){var
b=a(aa[2],0),d=a(af[17],b);return P(bl[10],b,d,0,c)[1]}function
Z(e,d,b){var
f=a(bm[1],b),g=[0,[0,c(bm[2],d,f)]],h=[0,[0,j_(bn[2],0,cT,0,0,g,0,b)],cS],i=a(l[1][6],e),j=F(bn[3],0,0,i,0,h);return a(X[15],j)}var
bo=[0,[0]],cX=[0,bp,0],cZ=[0,function(d,b){var
e=a(h[17][5],d),f=a(j[6],v[3]),i=c(g[13][2][7],f,e);function
k(d){var
e=b[1],f=a(x[22],d),g=c(x[17],cY,f),h=a(l[1][6],g);return c(l[1][11][22],h,e)}bo[1]=c(h[19][2],i,k);return a(s[16],0)}];e(g[4][16],0,bp,cZ);function
bq(F,E,k,b,D){function
G(h,b){var
d=b[1],i=b[3],j=b[2],k=a(x[22],d),m=c(x[17],c0,k),f=a(l[1][6],m),n=[2,[1,c(c1[1],0,f)]],o=a(g[13][2][1],h);return[0,d+1|0,[0,n,j],e(l[1][11][4],f,o,i)]}var
n=e(h[17][16],G,D,[0,0,0,l[1][11][1]]),H=n[3],J=n[2],K=[0,H,a(g[13][31],0)[2]];function
L(b){var
d=a(x[22],b),e=c(x[17],c2,d);return a(l[1][6],e)}var
M=c(h[17][56],k,L),N=a(j[5],v[3]),O=[0,cX,[0,[0,c(j[7],N,k)],0]],Q=[31,c(I[11],0,O)];function
R(a){return[0,a]}var
S=[5,[28,[0,c(h[17][69],R,M),Q]]],z=af[2][1],A=i[14],B=a(cU[11],F),f=P(cV[3][6],E,B,A,z),C=[0,f[1],f[3]],d=q(b),T=c(h[18],J,[0,S,0]),t=r===d?b[1]:m===d?a(p[2],b):b,u=[0,[0,c(I[11],0,t)],T],w=[3,c(I[11],0,u)],y=[29,c(I[11],0,w)],U=c(g[13][23],K,y),V=c(s[71][7],U,C),W=a(c3[2],V),o=a(af[156],W);function
X(c){var
b=a(g[13][2][2],c);return b?e(i[5],0,o,b[1]):a(x[3],cW)}var
Y=a(af[141],o);return[0,c(h[19][15],X,bo[1]),Y]}function
bs(b){return[m,function(f){var
c=e(O[4],c4,br,b),d=a(aU[21],c);return a(i[8],d)}]}function
am(a){return[m,function(b){return e(O[4],c5,br,a)}]}var
c7=bs(c6),aV=am(c8),aW=am(c9),R=bs(c_),ag=am(c$),ah=am(da);function
ai(b,d){var
c=q(b),e=r===c?b[1]:m===c?a(p[2],b):b;return a(i[21],[0,e,d])}function
t(d,b,g){var
e=q(b),h=r===e?b[1]:m===e?a(p[2],b):b,f=c(Y[9],d[1],h),j=f[2];d[1]=f[1];return a(i[21],[0,j,g])}function
bt(f,l){var
d=c(i[3],f,l);if(9===d[0]){var
b=d[2],m=d[1];if(2<=b.length-1){var
n=[0,m,e(h[19][7],b,0,b.length-1-2|0)],g=a(i[21],n);if(c(i[108][16],f,g)){var
j=b.length-1-1|0,k=b.length-1-2|0,o=G(b,j)[j+1];return[0,g,G(b,k)[k+1],o]}return L(dc)}}return L(db)}var
ao=[0,dd,[0,an,0]];function
df(a){return c(h[18],ao,a)}var
bu=c(h[17][69],df,de);function
S(b){return[m,function(f){var
c=e(O[4],dg,bu,b),d=a(aU[21],c);return a(i[8],d)}]}function
o(a){return[m,function(b){return e(O[4],dh,bu,a)}]}var
dk=c(h[17][69],l[1][6],[0,dj,[0,an,di]]),dl=a(l[5][4],dk);function
bv(b){return[m,function(d){var
c=a(l[6][4],b);return e(l[13][1],[0,dl],l[5][6],c)}]}function
ab(a){var
b=[0,an,dn];return[m,function(c){return e(O[2],dm,b,a)}]}var
ap=S(dp),dr=o(dq),dt=o(ds),dv=o(du),aq=S(dw),dy=S(dx),ar=S(dz),dB=S(dA),as=S(dC),dE=S(dD),dG=S(dF),T=bv(dH),dJ=o(dI),w=o(dK);function
bw(g,d,f){var
b=f;for(;;){var
e=c(i[3],d,b);if(6===e[0]){var
b=e[3];continue}var
j=bt(d,b)[1],k=function(c){var
b=c[1],d=q(b),e=c[2],f=r===d?b[1]:m===d?a(p[2],b):b;return[0,f,e]},l=c(h[17][69],k,g),n=function(a){return-1===a?1:2},o=[0,[0,cA(d,j),n],l];return function(a){return bg(o,a)}}}var
dL=0;function
dM(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
dO=[0,[0,ab(dN),dM],dL];function
dP(b){var
a=b+1|0;if(!(18<a>>>0))switch(a){case
12:case
17:return 2;case
0:case
9:case
10:case
11:case
14:case
16:case
18:return 0}return 1}var
dR=[0,[0,ab(dQ),dP],dO];function
dS(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
dU=[0,[0,ab(dT),dS],dR];function
dV(a){return 0}var
dX=[0,[0,o(dW),dV],dU];function
dY(a){return 0}var
d0=[0,[0,o(dZ),dY],dX],d1=[0,[0,ah,function(a){return-1===a?0:1}],d0],d2=[0,[0,ag,function(a){return-1===a?0:2===a?2:1}],d1];aP(d3,function(a,b){return bw(d2,a,b)});var
ac=a(h[21][1],[0,X[80]]),aX=e(at[4],0,d4,ac[1]),aY=e(at[4],0,d5,U[22][1]);function
d6(a){return c(ac[22],a,aX[1])}function
ec(d){var
a=d[2],b=d[1],e=c(y[47],b,a[1]),f=c(y[47],b,a[2]),h=c(y[47],b,a[3]),i=c(y[47],b,a[4]),j=c(y[47],b,a[5]),k=c(y[47],b,a[6]),l=c(y[47],b,a[9]),m=c(y[47],b,a[10]),n=c(g[3][1],b,a[7]),o=c(g[3][1],b,a[8]),p=c(g[3][1],b,a[11]),q=c(g[3][1],b,a[12]);if(e===a[1])if(f===a[2])if(c(X[74],h,a[3]))if(i===a[4])if(j===a[5])if(k===a[6])if(l===a[9])if(m===a[10])if(n===a[7])if(o===a[8])if(p===a[11])if(q===a[12])return a;return[0,e,f,h,i,j,k,n,o,l,m,p,q]}function
by(b){var
a=b[2],c=b[1][1];aX[1]=e(ac[4],a[1],a,aX[1]);aY[1]=e(U[22][4],c,a,aY[1]);return 0}var
av=a(au[1],ed),ee=av[8],ef=av[7];function
eg(a){return[0,a]}function
eh(c,b){var
a=1===c?1:0;return a?by(b):a}var
ei=a(au[4],[0,av[1],by,av[3],eh,eg,ec,ef,ee]);function
bz(c,d,b,a){try{var
e=P(g[23][11],c,d[1],b,a),i=e[2],f=P(g[23][12],c,e[1],b,a),j=f[2],h=P(g[23][13],c,f[1],b,a),k=h[2];d[1]=h[1];var
l=ai(c7,[0,b,a,i,j,k]);return l}catch(a){a=A(a);if(a===H)return L(ej);throw a}}function
ek(h,g,f,e,d,c,b,a){return ai(dy,[0,h,g,f,e,d,c,b,a])}function
el(f,e,d,c,b,a){return ai(dB,[0,f,e,d,c,b,a])}function
em(j,b,k){var
f=k[5],o=k[4],l=k[3],n=k[2],h=k[1],v=c(i[3],b[1],f);if(9===v[0])if(1===v[2].length-1){var
F=q(R),aT=v[1],aU=r===F?R[1]:m===F?a(p[2],R):R;if(e(i[96],b[1],aT,aU)){var
aV=t(b,dr,[0,h]),aW=o?t(b,dt,[0,h,n,l,o[1]]):t(b,dv,[0,h,n,l]),G=e(aw[9],j,b[1],aV),aX=G[2],I=e(aw[9],j,G[1],aW),aY=I[2];b[1]=I[1];return[0,aX,aY]}}var
w=[0,[0,[0,[0,h,[0,f]]],[0,[0,[0,h,[0,f]]],0]],[0,[0,h,[0,f]]]],J=bz(a(aa[2],0),b,h,f);try{var
aS=c(g[23][14],w,n),x=aS}catch(a){a=A(a);if(a!==H)throw a;var
x=L(en)}var
s=x[2];try{var
aR=c(g[23][14],w,l),y=aR}catch(a){a=A(a);if(a!==H)throw a;var
y=L(eo)}var
u=y[2];if(o){var
z=o[1];try{var
as=c(g[23][14],[0,[0,[0,[0,h,[0,f]]],0],[0,[0,h,[0,f]]]],z),B=as}catch(a){a=A(a);if(a!==H)throw a;var
B=L(ep)}var
C=B[2],K=ek(h,n,l,z,f,s,u,C),M=a(d[3],eq),N=e(D[15],j,b[1],C),O=a(d[3],er),P=a(d[13],0),Q=a(d[3],es),S=e(D[15],j,b[1],u),T=a(d[3],et),U=a(d[13],0),V=a(d[3],eu),W=e(D[15],j,b[1],s),X=a(d[3],ev),Y=a(d[13],0),Z=a(d[3],ew),_=e(D[15],j,b[1],f),$=a(d[3],ex),ab=c(d[12],$,_),ac=c(d[12],ab,Z),ae=c(d[12],ac,Y),af=c(d[12],ae,X),ag=c(d[12],af,W),ah=c(d[12],ag,V),ai=c(d[12],ah,U),aj=c(d[12],ai,T),ak=c(d[12],aj,S),al=c(d[12],ak,Q),am=c(d[12],al,P),an=c(d[12],am,O),ao=c(d[12],an,N),ap=c(d[12],ao,M),aq=ad[6],ar=function(a){return c(aq,0,a)};c(bA[23],ar,ap);var
E=K}else{var
at=a(d[3],ey),au=e(D[15],j,b[1],u),av=a(d[3],ez),ax=a(d[13],0),ay=a(d[3],eA),az=e(D[15],j,b[1],s),aA=a(d[3],eB),aB=a(d[13],0),aC=a(d[3],eC),aD=e(D[15],j,b[1],f),aE=a(d[3],eD),aF=c(d[12],aE,aD),aG=c(d[12],aF,aC),aH=c(d[12],aG,aB),aI=c(d[12],aH,aA),aJ=c(d[12],aI,az),aK=c(d[12],aJ,ay),aL=c(d[12],aK,ax),aM=c(d[12],aL,av),aN=c(d[12],aM,au),aO=c(d[12],aN,at),aP=ad[6],aQ=function(a){return c(aP,0,a)};c(bA[23],aQ,aO);var
E=el(h,n,l,f,s,u)}return[0,J,E]}function
bB(h,g,f,e,d,c,b,a){return a?a[1]:em(h,g,[0,f,e,d,c,b])}function
bC(b){if(typeof
b==="number"){var
c=q(as);return r===c?as[1]:m===c?a(p[2],as):as}else
return 0===b[0]?ai(dE,[0,b[1]]):ai(dG,[0,b[1]])}function
bD(v,u,t,s,o,d){if(d){var
b=d[1];if(0===b[0])return a(g[9][3],b[1]);var
f=b[1],i=bE[3],j=function(a){return c(i,0,a)};return bj(c(h[17][69],j,f))}var
e=q(T),k=r===e?T[1]:m===e?a(p[2],T):T,l=[0,[0,c(I[11],0,k)],0],n=[3,c(I[11],0,l)];return[29,c(I[11],0,n)]}function
ax(c,b,a){return t(b,dJ,[0,F(_[2],0,0,c,b[1],a),a])}function
bF(d,b,l){var
f=q(w),n=r===f?w[1]:m===f?a(p[2],w):w,g=c(Y[9],b[1],n),j=g[2];b[1]=g[1];var
o=t(b,ah,[0,j]);function
s(c,a){return t(b,ag,[0,j,ax(d,b,c),a])}var
u=e(h[17][16],s,l,o),k=e(aw[9],d,b[1],u),v=k[2];b[1]=k[1];var
x=a(i[bX][1],v);return c(Y[41],b[1],x)}function
bG(o,b,e){var
f=q(w),s=r===f?w[1]:m===f?a(p[2],w):w,i=c(Y[9],b[1],s),j=i[2];b[1]=i[1];if(e){var
k=e[1],d=k[1],u=k[2];if(0===d[0])var
l=a(g[9][3],d[1]);else
var
v=d[1],x=bE[3],y=function(a){return c(x,0,a)},l=bj(c(h[17][69],y,v));return[0,l,t(b,aW,[0,j,ax(o,b,M(u))])]}var
n=q(T),z=r===n?T[1]:m===n?a(p[2],T):T,A=[0,c(I[11],0,z)],B=t(b,aV,[0,j]),C=[3,c(I[11],0,[0,A,0])];return[0,[29,c(I[11],0,C)],B]}function
bH(h,b,d){var
e=q(w),i=r===e?w[1]:m===e?a(p[2],w):w,f=c(Y[9],b[1],i),g=f[2];b[1]=f[1];return d?t(b,aW,[0,g,ax(h,b,M(d[1]))]):t(b,aV,[0,g])}function
bI(h,b,d){var
e=q(w),i=r===e?w[1]:m===e?a(p[2],w):w,f=c(Y[9],b[1],i),g=f[2];b[1]=f[1];return d?t(b,aW,[0,g,ax(h,b,M(d[1]))]):t(b,aV,[0,g])}function
bJ(u,N,av,M,au,K,at,as,an){var
P=K[2],Q=K[1],R=N[2],f=N[1],aw=c(h[18],ao,eH);a(O[3],aw);var
k=a(aa[2],0),ad=F(_[2],0,0,k,f,R),s=c(i[3],f,ad);if(9===s[0]){var
b=s[2],v=b.length-1-6|0;if(2<v>>>0)var
j=0;else{var
t=s[1];switch(v){case
0:var
w=q(ar),ae=b[1],af=b[2],ag=b[3],ah=b[4],ai=b[5],aj=b[6],ak=r===w?ar[1]:m===w?a(p[2],ar):ar;if(e(i[96],f,t,ak))var
d=[0,eF,ae,af,ag,ah,ai,0,0,aj],j=1;else
var
j=0;break;case
1:var
j=0;break;default:var
y=b[1],z=b[2],A=b[3],B=b[4],C=b[5],D=b[6],E=b[7],H=b[8],I=q(ap),al=r===I?ap[1]:m===I?a(p[2],ap):ap;if(e(i[96],f,t,al))var
d=[0,0,y,z,A,B,C,[0,D],[0,E],H],j=1;else{var
J=q(aq),am=r===J?aq[1]:m===J?a(p[2],aq):aq;if(e(i[96],f,t,am))var
d=[0,eG,y,z,A,B,C,[0,D],[0,E],H],j=1;else
var
j=0}}}}else
var
j=0;if(!j)var
d=L(eE);var
S=d[9],T=d[8],U=d[6],V=d[5],W=d[2],n=[0,f],ax=d[4],ay=d[3],az=d[1],X=bB(k,n,W,V,U,T,S,av),Y=X[1],aA=X[2],$=bG(k,n,at),aB=$[2],aC=$[1],aD=bH(k,n,as),aE=bI(k,n,an),aF=[0,Y,[0,aA,[0,R,[0,aB,[0,aD,[0,aE,[0,bC(M),0]]]]]]],aG=bv(eI),ab=bq(k,n[1],5,aG,aF),ac=ab[2],o=ab[1],aH=G(o,3)[4],aI=G(o,4)[5],aJ=a(l[1][8],u),aK=Z(c(x[17],aJ,eJ),ac,aH),aL=a(l[1][8],u),aM=Z(c(x[17],aL,eK),ac,aI),aN=bD(k,f,M,az,[0,ay,ax,V,U,T],au),aO=Q?a(g[9][3],Q[1]):eM,aP=P?a(g[9][3],P[1]):eL,aQ=e(i[5],0,f,W),aR=e(i[5],0,f,S),aS=e(i[5],0,f,Y),aT=G(o,0)[1],aU=G(o,2)[3],aV=a(ei,[0,aQ,aR,aS,G(o,1)[2],aU,aT,aN,aC,aK,aM,aO,aP]);c(bK[6],u,aV);return 0}function
bL(a){return typeof
a==="number"?0:0===a[0]?[0,M(a[1])]:[1,M(a[1])]}function
u(e,b,d){return a(aT[3],b[1])?(b[1]=[0,d],0):L(c(x[17],e,eN))}function
eW(q,p,o){var
l=bk(p),b=[0,0],d=[0,0],e=[0,0],f=[0,0],g=[0,0],i=[0,0],j=[0,0],k=[0,0],r=l[2],s=l[1];function
m(a){switch(a[0]){case
0:return u(eO,b,bL(a[1]));case
1:return u(eP,e,a[1]);case
2:return u(eQ,f,a[1]);case
3:return u(eR,g,a[1]);case
4:var
c=a[1],h=M(a[2]);return u(eS,d,[0,M(c),h]);case
5:return u(eT,j,[0,a[1],a[2]]);case
6:return u(eU,i,a[1]);default:return u(eV,k,a[1])}}c(h[17][11],m,o);var
a=b[1],n=a?a[1]:0;return bJ(q,[0,s,r],d[1],n,e[1],[0,f[1],g[1]],j[1],i[1],k[1])}function
bM(d,a,c){if(a)return a;var
b=bt(d,c);return[0,b[2],[0,b[3],0]]}function
bN(f,a,b,d){var
g=t(a,ah,[0,b]);function
i(d,c){return t(a,ag,[0,b,d,c])}var
j=e(h[17][16],i,d,g),c=e(aw[9],f,a[1],j),k=c[2];a[1]=c[1];return k}function
z(b){var
c=a(i[8],b);return a(g[13][2][1],c)}function
V(b){var
d=a(g[13][31],0);return c(g[13][2][6],d,b)}function
eX(a){var
b=z(a[2]),c=z(a[3]),d=z(a[4]),e=z(a[5]),f=z(a[6]),g=V(a[7]),h=V(a[8]),i=z(a[9]),j=z(a[10]),k=V([28,[0,eY,a[11]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,k,[0,V([28,[0,eZ,a[12]]]),0]]]]]]]]]]]}function
e0(J,I,G,E){function
b(n){var
b=a(bO[42][4],n),f=a(s[67][4],n);try{var
j=bM(b,G,E),l=[0,b];if(j){var
o=j[2],k=F(_[2],0,0,f,b,j[1]),p=function(g){var
h=F(_[2],0,0,f,b,g),c=1-F(bx[80],0,f,b,k,h);if(c){var
i=a(d[3],d7);return e(W[6],0,d8,i)}return c};c(h[17][11],p,o);try{var
C=d6(e(i[5],0,b,k)),m=C}catch(g){g=A(g);if(g!==H)throw g;var
q=a(d[3],d9),r=e(D[15],f,b,k),t=a(d[3],d_),u=a(d[13],0),v=a(d[3],d$),w=c(d[12],v,u),x=c(d[12],w,t),y=c(d[12],x,r),B=c(d[12],y,q),m=e(W[6],0,ea,B)}var
K=bN(f,l,a(i[8],m[1]),j),L=a(g[13][2][1],K),M=z(bF(f,l,I)),N=eX(m),O=c(h[18],N,[0,M,[0,L,0]]),P=c(g[13][2][8],J,O),Q=a(s[65][1],l[1]),R=c(s[18],Q,P);return R}throw[0,aS,eb]}catch(b){b=A(b);if(a(s[71][9],b))return c(s[21],0,b);throw b}}return a(s[67][9],b)}var
e3=c(h[17][69],l[1][6],[0,e2,[0,an,e1]]),e4=a(l[5][4],e3),e5=0;function
e6(b){var
a=b+1|0;if(!(16<a>>>0))switch(a){case
11:case
15:return 2;case
0:case
13:case
16:return 0}return 1}var
e8=[0,[0,o(e7),e6],e5];function
e9(b){var
a=b+1|0;if(!(14<a>>>0))switch(a){case
9:case
13:return 2;case
0:case
11:case
14:return 0}return 1}var
e$=[0,[0,ab(e_),e9],e8];function
fa(b){var
a=b+1|0;if(!(18<a>>>0))switch(a){case
12:case
17:return 2;case
0:case
9:case
10:case
11:case
14:case
16:case
18:return 0}return 1}var
fc=[0,[0,ab(fb),fa],e$];function
fd(a){if(11<=a)if(14<=a)var
b=15<=a?0:1;else{if(12!==a)return 2;var
b=1}else
var
b=-1===a?1:8<=a?1:0;return b?0:1}var
ff=[0,[0,ab(fe),fd],fc];function
fg(b){var
a=b+1|0;if(!(20<a>>>0))switch(a){case
13:case
18:return 2;case
0:case
10:case
11:case
12:case
15:case
17:case
19:case
20:return 0}return 1}var
fi=[0,[0,o(fh),fg],ff];function
fj(a){if(12<=a)if(15<=a)var
b=17<=a?0:1;else{if(13!==a)return 2;var
b=1}else
var
b=-1===a?1:9<=a?1:0;return b?0:1}var
fl=[0,[0,o(fk),fj],fi];function
fm(a){return 0}var
fo=[0,[0,o(fn),fm],fl];function
fp(a){return 0}var
fr=[0,[0,o(fq),fp],fo],fs=[0,[0,ah,function(a){return-1===a?0:1}],fr],ft=[0,[0,ag,function(a){return-1===a?0:2===a?2:1}],fs];aP(fu,function(a,b){return bw(ft,a,b)});var
fv=0;function
fw(b){var
a=b+1|0;if(!(15<a>>>0))switch(a){case
10:case
14:return 2;case
0:case
12:case
15:return 0}return 1}var
fy=[0,[0,o(fx),fw],fv];function
fz(a){return 0}var
fB=[0,[0,o(fA),fz],fy];function
fC(a){return 0}var
fE=[0,[0,o(fD),fC],fB],fF=[0,[0,ah,function(a){return-1===a?0:1}],fE],fG=[0,[0,ag,function(a){return-1===a?0:2===a?2:1}],fF];aP(fH,function(e,f){function
b(c){var
b=c[1],d=q(b),e=c[2],f=r===d?b[1]:m===d?a(p[2],b):b;return[0,f,e]}var
d=c(h[17][69],b,fG);return function(a){return bg(d,a)}});function
fJ(a,b,c){return aQ(fI,a,b,c)}c(fL[3],fK,fJ);var
ay=o(fM),az=o(fN),aA=o(fO),fQ=o(fP),fS=o(fR),fU=o(fT),aZ=e(at[4],0,fY,ac[1]),a0=e(at[4],0,fZ,U[22][1]);function
f0(a){return c(ac[22],a,aZ[1])}function
f9(d){var
a=d[2],b=d[1],e=c(y[47],b,a[1]),f=c(y[47],b,a[2]),h=c(y[47],b,a[5]),i=c(y[47],b,a[6]),j=c(y[47],b,a[7]),k=c(y[47],b,a[8]),l=c(y[47],b,a[9]),m=c(g[3][1],b,a[3]),n=c(g[3][1],b,a[4]),o=c(g[3][1],b,a[10]),p=c(g[3][1],b,a[11]);if(e===a[1])if(f===a[2])if(h===a[5])if(i===a[6])if(j===a[7])if(k===a[8])if(l===a[9])if(m===a[3])if(n===a[4])if(o===a[10])if(p===a[11])return a;return[0,e,f,m,n,h,i,j,k,l,o,p]}function
bP(b){var
a=b[2],c=b[1][1];aZ[1]=e(ac[4],a[1],a,aZ[1]);a0[1]=e(U[22][4],c,a,a0[1]);return 0}var
aB=a(au[1],f_),f$=aB[8],ga=aB[7];function
gb(a){return[0,a]}function
gc(c,b){var
a=1===c?1:0;return a?bP(b):a}var
gd=a(au[4],[0,aB[1],bP,aB[3],gc,gb,f9,ga,f$]);function
ge(f,b,j,d){var
h=c(i[3],f[1],d);if(9===h[0])if(1===h[2].length-1){var
l=q(R),s=h[1],t=r===l?R[1]:m===l?a(p[2],R):R;if(e(i[96],f[1],s,t)){var
u=a(O[36],0)[6],v=a(aU[21],u),w=[0,a(i[8],v),[0,b,b,j]];return a(i[21],w)}}bz(a(aa[2],0),f,b,d);var
n=[0,[0,[0,[0,b,[0,d]]],0],[0,[0,b,[0,d]]]];try{var
o=c(g[23][14],n,j),k=o}catch(a){a=A(a);if(a!==H)throw a;var
k=L(gf)}return k[2]}function
gz(n,bs,br){var
as=[0,0],at=[0,0],au=[0,0],av=[0,0],aw=[0,0],ax=[0,0],aB=[0,0],aC=[0,0],aD=[0,0];function
bo(b){if(0===b[0]){var
a=b[1];switch(a[0]){case
0:return u(gq,as,bL(a[1]));case
1:return u(gr,au,a[1]);case
2:return u(gs,av,a[1]);case
3:return u(gt,aw,a[1]);case
4:var
c=a[1],d=M(a[2]);return u(gu,at,[0,M(c),d]);case
5:return u(gv,aC,[0,a[1],a[2]]);case
6:return u(gw,aB,a[1]);default:return u(gx,aD,a[1])}}return u(gy,ax,M(b[1]))}c(h[17][11],bo,br);var
aE=as[1],R=aE?aE[1]:0,aF=aD[1],aG=aB[1],aH=aC[1],aI=aw[1],aJ=av[1],aK=au[1],aL=ax[1],bp=at[1],aR=c(h[18],ao,gg);a(O[3],aR);var
ah=bk(bs),s=ah[2],v=ah[1],k=a(aa[2],0),d=[0,v],aN=F(_[2],0,0,k,d[1],s),K=c(i[3],d[1],aN);if(9===K[0]){var
b=K[2],S=b.length-1-8|0;if(2<S>>>0)var
j=0;else{var
N=K[1];switch(S){case
0:var
T=b[1],U=b[2],V=b[3],W=b[4],Y=b[5],$=b[6],ab=b[7],ac=b[8],ad=q(aA),aO=r===ad?aA[1]:m===ad?a(p[2],aA):aA;if(e(ae[bd],d[1],aO,N))var
f=[0,fW,T,U,V,W,Y,0,0,$,ab,ac,t(d,fU,[0,T,U,V,W,Y,$,ab,ac,s])],j=1;else
var
j=0;break;case
1:var
j=0;break;default:var
y=b[1],z=b[2],A=b[3],B=b[4],C=b[5],D=b[6],E=b[7],H=b[8],I=b[9],J=b[10],af=q(ay),aP=r===af?ay[1]:m===af?a(p[2],ay):ay;if(e(ae[bd],d[1],aP,N))var
f=[0,0,y,z,A,B,C,[0,D],[0,E],H,I,J,t(d,fQ,[0,y,z,A,B,C,D,E,H,I,J,s])],j=1;else{var
ag=q(az),aQ=r===ag?az[1]:m===ag?a(p[2],az):az;if(e(ae[bd],d[1],aQ,N))var
f=[0,fX,y,z,A,B,C,[0,D],[0,E],H,I,J,t(d,fS,[0,y,z,A,B,C,D,E,H,I,J,s])],j=1;else
var
j=0}}}}else
var
j=0;if(!j)var
f=L(fV);var
P=f[11],ai=f[8],aj=f[6],ak=f[5],Q=f[2],aS=f[12],aT=f[10],aU=f[4],aV=f[3],aW=f[1],al=bB(k,d,Q,ak,aj,ai,P,bp),am=al[2],an=al[1];bJ(n,[0,d[1],aS],[0,[0,an,am]],R,aK,gh,aH,aG,aF);var
ap=bG(k,d,aH),aX=ap[2],aY=ap[1],aZ=bH(k,d,aG),a0=bI(k,d,aF),a1=ge(d,Q,aT,P),a2=[0,an,[0,am,[0,a1,[0,s,[0,aX,[0,aZ,[0,a0,[0,bC(R),0]]]]]]]],aM=[m,function(c){var
b=a(l[6][4],gi);return e(l[13][1],[0,e4],l[5][6],b)}],aq=bq(k,d[1],9,aM,a2),w=aq[2],o=aq[1],a3=G(o,3)[4],a4=G(o,4)[5],a5=G(o,5)[6],a6=G(o,6)[7];if(aL)var
a7=[0,e(i[5],0,v,aL[1])],a8=[0,G(o,8)[9],a7],ar=a(X[13],a8);else
var
ar=G(o,7)[8];var
a9=a(l[1][8],n),a_=Z(c(x[17],a9,gj),w,a3),a$=a(l[1][8],n),ba=Z(c(x[17],a$,gk),w,a4),bb=a(l[1][8],n),bc=Z(c(x[17],bb,gl),w,a5),be=a(l[1][8],n),bf=Z(c(x[17],be,gm),w,a6),bg=a(l[1][8],n),bh=Z(c(x[17],bg,gn),w,ar),bi=bD(k,v,R,aW,[0,aV,aU,ak,aj,ai],aK),bj=aJ?a(g[9][3],aJ[1]):gp,bl=aI?a(g[9][3],aI[1]):go,bm=e(i[5],0,v,Q),bn=a(gd,[0,bm,e(i[5],0,v,P),bi,aY,a_,ba,bc,bf,bh,bj,bl]);c(bK[6],n,bn);return 0}function
gA(a){var
b=z(a[2]),c=V(a[3]),d=V(a[4]),e=z(a[5]),f=z(a[7]),g=z(a[6]),h=z(a[8]),i=z(a[9]),j=V([28,[0,gB,a[10]]]);return[0,b,[0,c,[0,d,[0,e,[0,f,[0,g,[0,h,[0,i,[0,j,[0,V([28,[0,gC,a[11]]]),0]]]]]]]]]]}var
N=[0,cK,cJ,eW,aY,e0,gz,a0,function(K,J,I,G){function
b(n){var
b=a(bO[42][4],n),f=a(s[67][4],n);try{var
j=bM(b,I,G),l=[0,b],o=c(h[18],ao,f1);a(O[3],o);if(j){var
p=j[2],k=F(_[2],0,0,f,b,j[1]),q=function(g){var
h=F(_[2],0,0,f,b,g),c=1-F(bx[80],0,f,b,k,h);if(c){var
i=a(d[3],f2);return e(W[6],0,f3,i)}return c};c(h[17][11],q,p);try{var
E=f0(e(i[5],0,b,k)),m=E}catch(g){g=A(g);if(g!==H)throw g;var
r=a(d[3],f4),t=e(D[15],f,b,k),u=a(d[3],f5),v=a(d[13],0),w=a(d[3],f6),x=c(d[12],w,v),y=c(d[12],x,u),B=c(d[12],y,t),C=c(d[12],B,r),m=e(W[6],0,f7,C)}var
L=bN(f,l,a(i[8],m[1]),j),M=a(g[13][2][1],L),N=z(bF(f,l,J)),P=gA(m),Q=c(h[18],P,[0,N,[0,M,0]]),R=c(g[13][2][8],K,Q),S=a(s[65][1],l[1]),T=c(s[18],S,R);return T}throw[0,aS,f8]}catch(b){b=A(b);if(a(s[71][9],b))return c(s[21],0,b);throw b}}return a(s[67][9],b)}];aH(322,N,"Newring_plugin.Newring");a(gD[10],aj);var
gE=0;function
gF(b,c){return a(N[2],b)}var
gI=[0,[0,[0,gH,[1,[5,a(j[16],v[4])],gG,0]],gF],gE];function
gJ(b,a,d){return c(N[1],b,a)}var
gM=[0,gL,[1,[5,a(j[16],v[7])],gK,0]],gP=[0,[0,[0,gO,[1,[5,a(j[16],v[4])],gN,gM]],gJ],gI];F(g[10][8],aj,gQ,0,0,gP);function
aC(b){switch(b[0]){case
0:var
f=b[1];if(typeof
f==="number")return a(d[3],gR);else{if(0===f[0]){var
j=c(d[32],E[20],f[1]),k=a(d[3],gS);return c(d[12],k,j)}var
l=c(d[32],E[20],f[1]),m=a(d[3],gT);return c(d[12],m,l)}case
1:var
h=b[1];if(0===h[0]){var
n=h[1],o=a(d[3],gU),p=a(g[5][23],n),q=a(d[3],gV),r=a(d[13],0),s=a(d[3],gW),t=c(d[12],s,r),u=c(d[12],t,q),v=c(d[12],u,p);return c(d[12],v,o)}var
w=h[1],x=a(d[3],gX),y=e(d[39],d[13],E[11],w),z=a(d[3],gY),A=a(d[13],0),B=a(d[3],gZ),C=c(d[12],B,A),D=c(d[12],C,z),F=c(d[12],D,y);return c(d[12],F,x);case
2:var
G=b[1],H=a(d[3],g0),I=a(g[5][23],G),J=a(d[3],g1),K=a(d[13],0),L=a(d[3],g2),M=c(d[12],L,K),N=c(d[12],M,J),O=c(d[12],N,I);return c(d[12],O,H);case
3:var
P=b[1],Q=a(d[3],g3),R=a(g[5][23],P),S=a(d[3],g4),T=a(d[13],0),U=a(d[3],g5),V=c(d[12],U,T),W=c(d[12],V,S),X=c(d[12],W,R);return c(d[12],X,Q);case
4:var
Y=b[1],Z=c(d[32],E[20],b[2]),_=c(d[32],E[20],Y),$=a(d[3],g6),aa=c(d[12],$,_);return c(d[12],aa,Z);case
5:var
i=b[1];if(0===i[0]){var
ab=b[2],ac=i[1],ad=a(d[3],g7),ae=a(g[5][23],ac),af=a(d[3],g8),ag=a(d[13],0),ah=c(d[32],E[20],ab),ai=a(d[3],g9),aj=c(d[12],ai,ah),ak=c(d[12],aj,ag),al=c(d[12],ak,af),am=c(d[12],al,ae);return c(d[12],am,ad)}var
an=b[2],ao=i[1],ap=a(d[3],g_),aq=e(d[39],d[13],E[11],ao),ar=a(d[3],g$),as=a(d[13],0),at=c(d[32],E[20],an),au=a(d[3],ha),av=c(d[12],au,at),aw=c(d[12],av,as),ax=c(d[12],aw,ar),ay=c(d[12],ax,aq);return c(d[12],ay,ap);case
6:var
az=c(d[32],E[20],b[1]),aA=a(d[3],hb);return c(d[12],aA,az);default:var
aB=c(d[32],E[20],b[1]),aC=a(d[3],hc);return c(d[12],aC,aB)}}var
a1=a(j[3],hd),he=a(j[4],a1),aD=e(n[16],n[13],hf,he),hg=0,hh=0;function
hi(a,c,b){return[0,[0,a]]}var
hj=[6,n[18][1]],hl=[0,[0,[0,[0,0,[0,a(k[10],hk)]],hj],hi],hh];function
hm(b,a){return hn}var
hp=[0,[0,[0,0,[0,a(k[10],ho)]],hm],hl];function
hq(a,c,b){return[0,[1,a]]}var
hr=[6,n[18][1]],ht=[0,[0,[0,[0,0,[0,a(k[10],hs)]],hr],hq],hp];function
hu(e,a,d,c,b){return[1,[0,a]]}var
hw=[0,a(k[10],hv)],hx=[6,g[6][18]],hz=[0,a(k[10],hy)],hB=[0,[0,[0,[0,[0,[0,0,[0,a(k[10],hA)]],hz],hx],hw],hu],ht];function
hC(e,a,d,c,b){return[1,[1,a]]}var
hE=[0,a(k[10],hD)],hF=[1,[6,n[18][7]]],hH=[0,a(k[10],hG)],hJ=[0,[0,[0,[0,[0,[0,0,[0,a(k[10],hI)]],hH],hF],hE],hC],hB];function
hK(e,a,d,c,b){return[2,a]}var
hM=[0,a(k[10],hL)],hN=[6,g[6][18]],hP=[0,a(k[10],hO)],hR=[0,[0,[0,[0,[0,[0,0,[0,a(k[10],hQ)]],hP],hN],hM],hK],hJ];function
hS(e,a,d,c,b){return[3,a]}var
hU=[0,a(k[10],hT)],hV=[6,g[6][18]],hX=[0,a(k[10],hW)],hZ=[0,[0,[0,[0,[0,[0,0,[0,a(k[10],hY)]],hX],hV],hU],hS],hR];function
h0(b,a,d,c){return[4,a,b]}var
h1=[6,n[18][1]],h2=[6,n[18][1]],h4=[0,[0,[0,[0,[0,0,[0,a(k[10],h3)]],h2],h1],h0],hZ];function
h5(a,c,b){return[6,a]}var
h6=[6,n[18][1]],h8=[0,[0,[0,[0,0,[0,a(k[10],h7)]],h6],h5],h4];function
h9(f,b,e,a,d,c){return[5,[1,b],a]}var
h$=[0,a(k[10],h_)],ia=[1,[6,n[18][7]]],ic=[0,a(k[10],ib)],id=[6,n[18][1]],ig=[0,[0,[0,[0,[0,[0,[0,0,[0,a(k[10],ie)]],id],ic],ia],h$],h9],h8];function
ih(f,b,e,a,d,c){return[5,[0,b],a]}var
ij=[0,a(k[10],ii)],ik=[6,g[6][18]],im=[0,a(k[10],il)],io=[6,n[18][1]],iq=[0,[0,[0,[0,[0,[0,[0,0,[0,a(k[10],ip)]],io],im],ik],ij],ih],ig];function
ir(a,c,b){return[7,a]}var
is=[6,n[18][1]],iu=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(k[10],it)]],is],ir],iq]],hg]];e(n[21],aD,0,iu);function
iv(c,b,a){return aC}c(g[5][3],a1,iv);function
bQ(b){var
c=e(d[39],d[28],aC,b);return a(d[46],c)}var
aE=a(j[3],iw),ix=a(j[4],aE),bR=e(n[16],n[13],iy,ix),iz=0,iA=0;function
iB(d,a,c,b){return a}var
iD=[0,a(k[10],iC)],iF=[2,[6,aD],[0,a(k[10],iE)]],iH=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(k[10],iG)]],iF],iD],iB],iA]],iz]];e(n[21],bR,0,iH);function
iI(c,b,a){return bQ}c(g[5][3],aE,iI);var
iJ=0,iK=[0,aF[3]],iP=[0,[0,0,iO,function(i,b){var
f=a(d[22],iL);c(ad[7],0,f);var
g=N[4][1];function
h(i,b){var
f=c(bS[6],0,0),g=f[2],h=f[1],j=e(D[7],g,h,b[2]),k=a(d[3],iM),l=a(d[13],0),m=e(D[7],g,h,b[1]),n=a(d[3],iN),o=a(d[13],0),p=a(U[18],i),q=a(E[9],p),r=c(d[12],q,o),s=c(d[12],r,n),t=c(d[12],s,m),u=c(d[12],t,l),v=c(d[12],u,k),w=c(d[12],v,j),x=c(d[26],2,w);return c(ad[7],0,x)}c(U[22][10],h,g);return b},iK],iJ],iQ=0;function
iR(d,c,a,g,b){var
f=a?a[1]:0;e(N[3],d,c,f);return b}var
iT=[1,iS,[4,[5,a(j[16],aE)]],0],iW=[0,iV,[1,iU,[5,a(j[16],v[11])],iT]],i0=[0,[0,0,[0,iZ,[0,iY,[1,iX,[5,a(j[16],v[7])],iW]]],iR,iQ],iP],i1=0,i2=[0,function(a){return aF[4]}];P(bT[10],i3,i2,i1,i0);var
i4=0;function
i5(e,d,c,f){var
b=a(h[17][cj],c);return P(N[5],e,d,b[2],b[1])}var
i8=[0,i7,[1,[0,[5,a(j[16],v[11])]],i6,0]],i$=[0,i_,[1,[2,[5,a(j[16],v[11])]],i9,i8]],jc=[0,[0,[0,jb,[1,[6,a(j[16],g[2][8]),0],ja,i$]],i5],i4];F(g[10][8],aj,jd,0,0,jc);function
a2(b){if(0===b[0])return aC(b[1]);var
e=c(d[32],E[20],b[1]),f=a(d[3],je);return c(d[12],f,e)}var
a3=a(j[3],jf),jg=a(j[4],a3),a4=e(n[16],n[13],jh,jg),ji=0,jj=0,jk=[0,[0,[0,0,[6,aD]],function(a,b){return[0,a]}],jj];function
jl(a,c,b){return[1,a]}var
jm=[6,n[18][1]],jo=[0,0,[0,[0,0,0,[0,[0,[0,[0,0,[0,a(k[10],jn)]],jm],jl],jk]],ji]];e(n[21],a4,0,jo);function
jp(c,b,a){return a2}c(g[5][3],a3,jp);function
bU(b){var
c=e(d[39],d[28],a2,b);return a(d[46],c)}var
aG=a(j[3],jq),jr=a(j[4],aG),bV=e(n[16],n[13],js,jr),jt=0,ju=0;function
jv(d,a,c,b){return a}var
jx=[0,a(k[10],jw)],jz=[2,[6,a4],[0,a(k[10],jy)]],jB=[0,0,[0,[0,0,0,[0,[0,[0,[0,[0,0,[0,a(k[10],jA)]],jz],jx],jv],ju]],jt]];e(n[21],bV,0,jB);function
jC(c,b,a){return bU}c(g[5][3],aG,jC);var
jD=0,jE=[0,aF[3]],jJ=[0,[0,0,jI,function(i,b){var
f=a(d[22],jF);c(ad[7],0,f);var
g=N[7][1];function
h(i,b){var
f=c(bS[6],0,0),g=f[2],h=f[1],j=e(D[7],g,h,b[2]),k=a(d[3],jG),l=a(d[13],0),m=e(D[7],g,h,b[1]),n=a(d[3],jH),o=a(d[13],0),p=a(U[18],i),q=a(E[9],p),r=c(d[12],q,o),s=c(d[12],r,n),t=c(d[12],s,m),u=c(d[12],t,l),v=c(d[12],u,k),w=c(d[12],v,j),x=c(d[26],2,w);return c(ad[7],0,x)}c(U[22][10],h,g);return b},jE],jD],jK=0;function
jL(d,c,a,g,b){var
f=a?a[1]:0;e(N[6],d,c,f);return b}var
jN=[1,jM,[4,[5,a(j[16],aG)]],0],jQ=[0,jP,[1,jO,[5,a(j[16],v[11])],jN]],jU=[0,[0,0,[0,jT,[0,jS,[1,jR,[5,a(j[16],v[7])],jQ]]],jL,jK],jJ],jV=0,jW=[0,function(a){return aF[4]}];P(bT[10],jX,jW,jV,jU);var
jY=0;function
jZ(e,d,c,f){var
b=a(h[17][cj],c);return P(N[8],e,d,b[2],b[1])}var
j2=[0,j1,[1,[0,[5,a(j[16],v[11])]],j0,0]],j5=[0,j4,[1,[2,[5,a(j[16],v[11])]],j3,j2]],j8=[0,[0,[0,j7,[1,[5,a(j[16],g[2][8])],j6,j5]],jZ],jY];F(g[10][8],aj,j9,0,0,j8);var
bW=[0,aj,aC,a1,aD,bQ,aE,bR,a2,a3,a4,bU,aG,bV];aH(330,bW,"Newring_plugin.G_newring");aH(331,[0,be,N,bW],"Newring_plugin");return}

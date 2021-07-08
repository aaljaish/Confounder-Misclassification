/* Downloaded from: https://support.sas.com/kb/66/addl/fusion_66969_6_ranmbin.sas.txt */

%macro RanMBin(version, inmeans=, means=, incorr=, exch=, ar=, k=, seed=0, 
               out=Mbin, n=100, subject=ID, within=SubID, outshape=wide);

%local notesopt m vio mnerr errncorr invcorr ark rho shape i j nnv varlist;
%let vio=0; %let mnerr=0; %let errcorr=0; %let ark=0;

%let time = %sysfunc(datetime());
%let _version=1.0;
%if &version ne %then %put NOTE: &sysmacroname macro Version &_version..;
%let notesopt = %sysfunc(getoption(notes));
%let version=%upcase(&version);
%if %index(&version,DEBUG) %then %do;
  options notes mprint
    %if %index(&version,DEBUG2) %then mlogic symbolgen;
  ;  
  ods select all;
  %put _user_;
%end;
%else %do;
  options nonotes nomprint nomlogic nosymbolgen;
  ods exclude all;
%end;

/* Check for newer version */
%let _notfound=0;
filename _ver url 'http://ftp.sas.com/techsup/download/stat/versions.dat' 
         termstr=crlf;
data _null_;
  infile _ver end=_eof;
  input name:$15. ver;
  if upcase(name)="&sysmacroname" then do;
    call symput("_newver",ver); stop;
  end;
  if _eof then call symput("_notfound",1);
  run;
%if &notesopt ne NONOTES %then options notes;;
%if &syserr ne 0 or &_notfound=1 %then
  %put NOTE: Unable to check for newer version of &sysmacroname macro.;
%else %if %sysevalf(&_newver > &_version) %then %do;
  %put NOTE: A newer version of the &sysmacroname macro is available at;
  %put NOTE- this location: http://support.sas.com/ ;
%end;
%if %index(%upcase(&version),DEBUG)=0 %then options nonotes;;

/* Input checks */
%if &incorr ne and %sysfunc(exist(&incorr))=0 %then %do;
  %put ERROR: INCORR= data set not found.;
  %goto exit;
%end;
%if &inmeans ne and %sysfunc(exist(&inmeans))=0 %then %do;
  %put ERROR: INMEANS= data set not found.;
  %goto exit;
%end;
%let shape=%substr(%upcase(&outshape),1,1);
%if &shape ne L and &shape ne W %then %do;
  %put ERROR: OUTSHAPE= must be LONG or WIDE.;
  %goto exit;
%end;
%if %quote(&exch) ne %then %let rho=%qscan(&exch,1,,s);
%if %quote(&ar) ne %then %let rho=%qscan(&ar,1,,s);
%let n=%sysfunc(max(1,%sysfunc(ceil(%qscan(&n,1,,s)))));
%if %quote(&means) ne and &inmeans ne %then %do;
  %if &notesopt ne NONOTES %then options notes;;
  %put NOTE: When both MEANS= and INMEANS= are specified, INMEANS= is ignored.;
  %let inmeans=;
  %if %index(&version,DEBUG)=0 %then options nonotes;;
%end;
%if %quote(&means)= and &inmeans= %then %do;
  %put ERROR: MEANS= or INMEANS= is required.;
  %goto exit;
%end;
/*
%if &incorr ne and &inmeans= %then %do;
  %put ERROR: When INCORR= is specified, INMEANS= is required.;
  %goto exit;
%end;
*/
%if &inmeans ne and &incorr= and %quote(&exch)= and %quote(&ar)= %then %do;
  %put ERROR: EXCH=, AR=, or INCORR= is required.;
  %goto exit;
%end;
%if (%eval(&incorr ne) + %eval(%quote(&exch) ne) + %eval(%quote(&ar) ne))>1 %then %do;
  %put ERROR: Specify only one of EXCH=, AR=, or INCORR=.;
  %goto exit;
%end;
%if %quote(&exch) ne and %quote(&k) ne %then %do;
  %put NOTE: When EXCH= is specified, K= is ignored.;
%end;

%if %quote(&means) ne and &inmeans= %then %do;
   %let i=1; %let m=0;
   %do %while ( %qscan(&means,&i,,s) ne );
      %if %index(%qscan(&means,&i,,s),*)=0 %then %let m=%eval(&m+1);
      %else %let m=%eval(&m+%qscan(%qscan(&means,&i,,s),1,*));
      %let i=%eval(&i+1);
   %end;
   data _null_; 
     array m (&m) (&means); 
     call symput("means",catx(' ',of m:)); 
     run;
%end;

%if &inmeans ne and %quote(&means)= %then %do;
  %if %sysfunc(exist(&inmeans)) %then %do;
    data _null_; set &inmeans(obs=1);
      call symput("means",catx(' ',of _numeric_));
      run;
    %let m=%sysfunc(countw(&means,,s));
  %end;
  %else %do;
    %put ERROR: Data set %upcase(&inmeans) not found.;
    %goto exit;
  %end;
%end;

%if &m<=1 %then %do;
  %put ERROR: More than 1 mean value must be specified.;
  %goto exit;
%end;
%if &k ne and &k<1 or &k>(&m-1) %then %do;
  %put ERROR: K= must be a positive integer between 1 and %eval(&m-1).;
  %goto exit;
%end;

%if &incorr ne %then %do;
  %if %sysfunc(exist(&incorr)) %then %do;
    %let dsid=%sysfunc(open(&incorr));
    %let varlist=; %let nnv=0;
    %do i=1 %to %sysfunc(attrn(&dsid, nvars));
       %if %sysfunc(vartype(&dsid, &i))=N %then %do;
         %let nnv=%eval(&nnv+1); %if &nnv>1 %then 
         %let varlist=&varlist %sysfunc(varname(&dsid, &i));
       %end;
    %end;
    %if &nnv ne &m %then %do;
      %put ERROR: The number of means, &m, must equal the number of correlation;
      %put ERROR- matrix variables, &nnv..;
      %goto exit;
    %end;
    %let errncorr=0; %let invcorr=0;
    %let rc=%sysfunc(close(&dsid));
    data _unstr;
      array _x (&m) (&means);
      %if %index(&version,DEBUG)=0 %then  keep _x: _r:; ;
      array _r (%eval(&m-1),%eval(&m-1));
      do _i=1 to %eval(&m-1);
        set &incorr point=_i;
        array _c (*) &varlist;
        if dim(_c) ne %eval(&m-1) then call symput("errncorr","1");
        do _j=1 to %eval(&m-1);
          _r(_i,_j)=_c(_j);
        end; 
      end;
      output;
      stop;
      run;
    %if &errncorr %then %do;
      %put ERROR: The INCORR= data set must have &m numeric variables and &m observations.;
      %goto exit;
    %end;
  %end;
  %else %do;
    %put ERROR: Data set %upcase(&incorr) not found.;
    %goto exit;
  %end;
%end;

/* Check Prentice constraints for Exchangeable or AR(1) */
%if %quote(&exch) ne or (%quote(&ar) ne and &k=) %then %do;
   data vio
        %if %index(&version,DEBUG)=0 %then (keep=vio corr rmin rmax);
        ;
      array p (&m) (&means);
      array r (1:%eval(&m-1),2:&m) (%eval((&m-1)*(&m-1))*&rho);
      do i=1 to &m-1;
      do j=i+1 to &m;
         if p(i)<=0 or p(i)>=1 or p(j)<=0 or p(j)>=1 then do;
            put "ERROR: Mean values must be greater than zero and less than one.";
            call symput("mnerr","1");
            stop;
         end;
         mij=( p(i)*(1-p(j)) ) / ( p(j)*(1-p(i)) );
         mji=1/mij;
         Rmin=0; 
         Rmax=min(sqrt(mij/mji),sqrt(mji/mij));
         Corr=r(i,j)
           %if %quote(&ar) ne %then **(j-i);
         ; 
         if corr<rmin or corr>rmax then do;
           vio+1; output;
         end; 
         %if %index(&version,DEBUG) %then %do; 
           if rmin<=corr<=rmax then output;
         %end;
      end; end;
      if vio then do;  
         put "ERROR: " vio "violations of Prentice constraints on the correlations";
         call symput("vio",cats(vio));
      end;
   run;
   %if &mnerr %then %goto exit;
%end;

/* Exchangeable */
%if %quote(&exch) ne and &vio=0 %then %do;
   data &out
        %if %index(&version,DEBUG)=0 %then %do;
             (keep=&subject   
                  %if &shape=L %then Y &within;
                  %else Y:;
             )
        %end;
        ;
      array p (&m) (&means);
      array _y (&m) Y1-Y&m;
      _pmin=min(of p1-p&m); 
      _pmax=max(of p1-p&m);
      _g=sqrt(_pmin*_pmax)/(sqrt(_pmin*_pmax)+sqrt((1-_pmin)*(1-_pmax)));
      call streaminit(&seed);
      do &subject=1 to &n;
        _z=rand("bernoulli",_g);
        do _j=1 to &m;
          _a=sqrt( (&rho*p(_j)*(1-p(_j))) / (_g*(1-_g)) );
          _b=(p(_j)-_a*_g)/(1-_a);
          _u=rand("bernoulli",_a);
          _yy=rand("bernoulli",_b);
          _y(_j)=(1-_u)*_yy+_u*_z;
          %if &shape=L %then %do;
            Y=_y(_j); &within=_j;
            output;
          %end;
        end;
        %if &shape=W %then output;;
      end;
   run;
%end;

/* AR(1) */
%if %quote(&ar) ne and &k ne %then %let ark=1;
%if %quote(&ar) ne and &k= and &vio=0 %then %do;
   data &out
        %if %index(&version,DEBUG)=0 %then %do;
             (keep=&subject  
                  %if &shape=L %then Y &within;
                  %else Y:;
             )
        %end;
        ;
      array p (&m) (&means);
      array _y (&m) Y1-Y&m;
      call streaminit(&seed);
      do &subject=1 to &n;
        _y(1)=rand("bernoulli",p(1));
        %if &shape=L %then %do;
          Y=_y(1); &within=1;
          output;
        %end;
        do j=2 to &m;
          a=&rho*sqrt( (p(j)*(1-p(j))) / (p(j-1)*(1-p(j-1))) );
          b=(p(j)-a*p(j-1))/(1-a);
          u=rand("bernoulli",a);
          _yy=rand("bernoulli",b);
          _y(j)=(1-u)*_yy+u*_y(j-1);
          %if &shape=L %then %do;
            Y=_y(j); &within=j;
            output;
          %end;
        end;
        %if &shape=W %then output;;
      end; 
   run;
%end;

/* Unstructured */
%if &incorr ne or &ark %then %do;
   %if &ark %then %do;
      data _unstr;
         array _x (&m) (&means); 
         array _r (1:%eval(&m-1),2:&m) (%eval((&m-1)*(&m-1))*0);
         do _i=1 to &m-1;
         do _j=_i+1 to &m;
           if (_j-_i)<=&k then _r(_i,_j)=&rho**(_j-_i);
         end; end;
         %if %index(&version,DEBUG)=0 %then  keep _x: _r:; ;
         output;
         run;
   %end;
   %if &k= %then %let k=%eval(&m-1);
   data &out
        %if %index(&version,DEBUG)=0 %then %do;
             (keep=&subject 
                  %if &shape=L %then Y &within;
                  %else Y:;
             ) 
        %end;
        vio
           %if %index(&version,DEBUG)=0 %then (keep=vio corr rmin rmax);
      ;
      set _unstr;
      array p (&m) _x:;
      array r (1:%eval(&m-1),2:&m) _r:;               *row-major order;
      array rd (%eval(&m-1),&m) (%eval((&m-1)*&m)*0); *diagonal order;
      array b (&k,&m);
      array bcol (&m) (&m*1);
      array bdiag (&m) (&m*1);
      array _y (&k,&m);
      array _ycol (&m) (&m*1);
      array _ydiag (&m) (&m*1);
      array a (&m);
      array u (&m);
      array _yy (&m) Y1-Y&m;
      /* Check Prentice constraints */
      do i=1 to &m-1;
      do j=i+1 to &m;
        if p(i)<=0 or p(i)>=1 or p(j)<=0 or p(j)>=1 then do;
           put "ERROR: Mean values must be greater than zero and less than one.";
           call symput("mnerr","1");
           stop;
        end;
        mij=( p(i)*(1-p(j)) ) / ( p(j)*(1-p(i)) );
        mji=1/mij;
        Rmin=0;
        Rmax=min(sqrt(mij/mji),sqrt(mji/mij));
        Corr=r(i,j); 
        if Corr<rmin or Corr>rmax then do;
          vio+1; output vio;
        end;
        %if %index(&version,DEBUG) %then %do; 
          if rmin<=Corr<=rmax then output;
        %end;
        rd(j-i,i)=r(i,j);  
      end; end;
      if vio then do;
        put "ERROR: " vio "violations of Prentice constraints on the correlations";
        call symput("vio",cats(vio));
        stop;
      end;
      /* Generate alpha (a) and beta (b) components and check for invalid value */
      do i=1 to &k;
      do j=1 to &m;
        if (i+j)<=&m then pij=p(i+j); else pij=p(&m);
        b(i,j)=(p(j)*pij) / ( p(j)*pij + rd(i,j)*sqrt(p(j)*pij*(1-p(j))*(1-pij)) );
        bcol(j)=bcol(j)*b(i,j);
        if (i+j)<=&m then bdiag(i+j)=bdiag(i+j)*b(i,j);
      end; end;
      do i=1 to &m;
        k=min(i-1,&k);
        a(i)=p(i)/(bcol(i)*bdiag(i)); 
        if a(i)>1 then do;
          put "ERROR: Specification not feasible. a<1 required, but a=" a(i) "for variable " i ".";
          put "ERROR- Try altering means and/or correlations.";
          %if %index(&version,DEBUG) %then output;;
         stop;
        end;
      end;
      /* Generate correlated binary variables */
      call streaminit(&seed);
      do &subject=1 to &n;
        do j=1 to &m; _ycol(j)=1; _ydiag(j)=1; end;
        do i=1 to &k;
        do j=1 to &m;
          _y(i,j)=rand("bernoulli",b(i,j));
          _ycol(j)=_ycol(j)*_y(i,j);
          if (i+j)<=&m then _ydiag(i+j)=_ydiag(i+j)*_y(i,j); 
        end; 
        end;
        do i=1 to &m;
          u(i)=rand("bernoulli",a(i));
          _yy(i)=u(i)*_ycol(i)*_ydiag(i);
          %if &shape=L %then %do;
            Y=_yy(i); &within=i;
            output &out;
          %end;
        end;
        %if &shape=W %then output &out;;
      end;
   run;
%end; 
%if &mnerr %then %goto exit;

%if &vio %then %do;
   ods select all;
   proc print data=vio noobs label;
     id vio;
     var corr rmin rmax;
     label vio="Violation";
     title "Violations of Prentice constraints";
     title2 "Rmin <= Corr <= Rmax";
     run;
%end;
%else %do;
   %if &notesopt ne NONOTES %then options notes;;
   data &out; set &out; run;
   %if %index(&version,DEBUG)=0 %then options nonotes;;  
%end;

%exit:
  /* if want to keep temp data sets when debugging */
  %if %index(&version,DEBUG)=0 %then %do;  
   proc datasets nolist nowarn;
     delete _unstr;
     run; quit;
  %end;
  %if %index(&version,DEBUG) %then %do;
   options nomprint nomlogic nosymbolgen;
   %put _user_;
  %end;
  ods select all;
  options &notesopt;

  %let time = %sysfunc(round(%sysevalf(%sysfunc(datetime()) - &time), 0.01));
  %put NOTE: The &sysmacroname macro used &time seconds.;
  title;
%mend;

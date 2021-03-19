%include polycode.fmt
%include linear.fmt
%include main.fmt
\begin{figure*}
\def\sep{\thickspace\thickspace\thickspace}%
\begin{mathpar}
  \inferrule*{
    %[lab=T-Var]
  }{\pgv{\seq{\top}{\bot}{\tmty{x}{T}}{x}{T}}}
  =
  \inferrule*{
    |x :: (ToSesh T)|\vdash|x :: (ToSesh T)|
  }{|ireturn x :: Sesh Top Bot (ToSesh T)|}
  \\

  \inferrule*{
    %[lab=T-Lam]
    \pgv{\seq{p}{q}{\ty{\Gamma},\tmty{x}{T}}{L}{U}}
  }{\pgv{\seq{\top}{\bot}{\ty{\Gamma}}{\lambda x.L}{\tylolli{p}{q}{T}{U}}}}
  =
  \inferrule*{
    |ToSesh Gamma, x :: ToSesh T|\vdash|tosesh L :: Sesh p q (ToSesh U)|
  }{|ireturn (\x -> tosesh L) :: Sesh Top Bot (ToSesh T %1 -> Sesh p q (ToSesh U))|}
  \\

  \inferrule*{
    %[lab=T-Const]
  }{\pgv{\seq{{\top}}{{\bot}}{\emptyenv}{K}{T}}}
  =
  \inferrule*{
  }{|ireturn (tosesh K) :: Sesh Top Bot (ToSesh T)|}
  \\

  \inferrule*{
    %[lab=T-App]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\tylolli{p''}{q''}{T}{U}}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta}}{M}{T}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
    \sep
    \pgv{\cs{q'}<\cs{p''}}
  }{\pgv{\seq{{p}\sqcap{p'}\sqcap{p''}}{{q}\sqcup{q'}\sqcup{q''}}{\ty{\Gamma},\ty{\Delta}}{L\;M}{U}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh T %1 -> Sesh p'' q'' (ToSesh U))|
    \sep
    |ToSesh Delta|\vdash|tosesh M :: Sesh p' q' (ToSesh T)|
  }{|tosesh L >>>= \f -> tosesh M >>= \x -> f x :: (LT q p', LT q' p'') => Sesh (p `Min` p' `Min` p'') (q `Max` q' `Max` q'') (ToSesh U)|}
  \\

  \inferrule*{
    %[lab=T-Unit]
  }{\pgv{\seq{{\top}}{{\bot}}{\emptyenv}{\unit}{\tyunit}}}
  =
  \inferrule*{
  }{|ireturn () :: Sesh Top Bot ()|}
  \\

  \inferrule*{
    %[lab=T-LetUnit]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\tyunit}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta}}{M}{T}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
  }{\pgv{\seq{{p}\sqcap{p'}}{{q}\sqcup{q'}}{\ty{\Gamma},\ty{\Delta}}{\letunit{L}{M}}{T}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q ()|
    \sep
    |ToSesh Delta|\vdash|tosesh M :: Sesh p' q' (ToSesh T)|
  }{|tosesh L >>>= \() -> M :: (LT p q') => Sesh (Min p p') (Max q q') (ToSesh T)|}
  \\

  \inferrule*{
    %[lab=T-Pair]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{T}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta}}{M}{U}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
  }{\pgv{\seq{{p}\sqcap{p'}}{{q}\sqcup{q'}}{\ty{\Gamma},\ty{\Delta}}{\pair{L}{M}}{\typrod{T}{U}}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh T)|
    \sep
    |ToSesh Delta|\vdash|tosesh M :: Sesh p' q' (ToSesh U)|
  }{|tosesh L >>>= \x -> tosesh M >>= \y -> ireturn (x,y) :: (LT q p') => Sesh (Min p p') (Max q q') (ToSesh T, ToSesh U)|}
  \\

  \inferrule*{
    %[lab=T-LetPair]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\typrod{T}{T'}}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta},\tmty{x}{T},\tmty{y}{T'}}{M}{U}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
  }{\pgv{\seq{{p}\sqcap{p'}}{{q}\sqcup{q'}}{\ty{\Gamma},\ty{\Delta}}{\letpair{x}{y}{L}{M}}{U}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh T, ToSesh T')|
    \sep
    |ToSesh Delta, x :: ToSesh T, y :: ToSesh T'|\vdash|tosesh M :: Sesh p' q' (ToSesh U)|
  }{|tosesh L >>>= \(x,y) -> tosesh M :: (LT q p') => Sesh (Min p p') (Max q q') (ToSesh U)|}
  \\

  \inferrule*{
    %[lab=T-Inl]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{T}}
    % \sep
    % \pgv{\cs{\pr(\ty{T})}=\cs{\pr(\ty{U})}}
  }{\pgv{\seq{p}{q}{\ty{\Gamma}}{\inl{L}}{\tysum{T}{U}}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh T)|
  }{|ToSesh Gamma|\vdash|tosesh L >>>= \x -> ireturn (Left x) :: Sesh p q (Either (ToSesh T) (ToSesh U))|}
  \\

  \inferrule*{
    %[lab=T-Inr]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{T}}
    % \sep
    % \pgv{\cs{\pr(\ty{T})}=\cs{\pr(\ty{U})}}
  }{\pgv{\seq{p}{q}{\ty{\Gamma}}{\inr{L}}{\tysum{T}{U}}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh U)|
  }{|ToSesh Gamma|\vdash|tosesh L >>>= \x -> ireturn (Right x) :: Sesh p q (Either (ToSesh T) (ToSesh U))|}
  \\

  \inferrule*{
    %[lab=T-CaseSum]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\tysum{T}{T'}}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta},\tmty{x}{T}}{M}{U}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta},\tmty{y}{T'}}{N}{U}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
  }{\pgv{\seq{{p}\sqcup{p'}}{{q}\sqcup{q'}}{\ty{\Gamma},\ty{\Delta}}{\casesum{L}{x}{M}{y}{N}}{U}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (Either (ToSesh T) (ToSesh T'))|
    \sep
    |ToSesh Delta|\vdash|x :: ToSesh T|\vdash|tosesh M :: Sesh p' q' (ToSesh U)|
    \sep
    |ToSesh Delta|\vdash|y :: ToSesh T'|\vdash|tosesh N :: Sesh p' q' (ToSesh U)|
  }{|ToSesh Gamma, ToSesh Delta|\vdash|tosesh L >>>= \x -> case x { Left x -> tosesh M; Right y -> tosesh N } :: Sesh (Min p p') (Max q q') (ToSesh U)|}
  \\

  \inferrule*{
    %[lab=T-Absurd]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\tyvoid}}
  }{\pgv{\seq{p}{q}{\ty{\Gamma}}{\absurd{L}}{T}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q Void|
  }{|ToSesh Gamma|\vdash|tosesh L >>>= \x -> absurd x :: Sesh p q (ToSesh T)|}
\end{mathpar}
\caption{Translation from Priority GV to Sesh preserves types.}
\label{fig:pgv-to-sesh-typing}
\end{figure*}

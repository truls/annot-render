{-# LANGUAGE QuasiQuotes #-}

module LatexSnippets where

--import           Text.Diff.Parse.Types

import           Text.RawString.QQ

preamble :: String
preamble  = [r| \documentclass{article}
\usepackage{listings}
\usepackage{tikz}
\usetikzlibrary{arrows,tikzmark,shadows,calc}
\usetikzmarklibrary{listings}

\tikzset{
  comment/.style={
    draw,
    fill=blue!70,
    text=white,
    rounded corners,
    drop shadow,
    align=left,
  },
}

\usepackage{eso-pic}
\usepackage[calc]{picture}
\newcommand{\addstufftoforegroundall}[1]{%
  \AddToShipoutPictureFG{% Add <stuff> to all following pages' foreground
    \put(0,\paperheight){\vtop{{\null}\makebox[0pt][l]{#1}}}%
  }%
}%
\newcommand{\addstufftoforegroundthis}[1]{%
  \AddToShipoutPictureFG*{% Add <stuff> to the current page foreground
    \put(0,\paperheight){\vtop{{\null}\makebox[0pt][l]{#1}}}%
  }%
}%

\setcounter{errorcontextlines}{\maxdimen}


\makeatletter
\def\getpicturepage#1{%
  \@nameuse{save@pg@\@nameuse{save@pt@#1}}%
}

\newcommand*{\iftikzmarkcurrentpage}[3]{%
    \iftikzmark{#1}{%
        \ifcsname save@pg@\@nameuse{save@pt@#1}\endcsname
            \expandafter\ifnum\getpicturepage{#1}=\the\c@page\relax
                #2%
            \else
                #3%
            \fi
        \else
            #3%
        \fi
    }{%
        #3
    }%
}

\newcommand*{\appendhookorcreatenew}[2]{%#1<- hook csname, #2<- code to add
    % note: code in #2 is not expanded at this stage!
    \ifcsname #1\endcsname
        \expandafter\g@addto@macro\csname #1\endcsname{\unexpanded{#2}}%
        \else
        \@namedef{#1}{\unexpanded{#2}}%
    \fi
}
\newcommand*{\deferlinecode}[3]{% #1<-listing name, #2<-line number, #3<-code
    \appendhookorcreatenew{listings-deferline-#1-#2}{#3}%
}
\newcommand*{\atEOLcode}[3]{% #1<-listing name, #2<-line number, #3<-code
    \appendhookorcreatenew{listings-eol-execute-#1-#2}{#3}%
}
\newcommand*{\commenton}[4][comment,thin]{% #1<- node keys, #2<- listing name, #3<- line number, #4<- comment text
    \deferlinecode{#2}{#3}{\tikz[overlay,remember picture] \draw[,>=stealth',<-,ultra thick] (pic cs:line-#2-#3-end) +(1em,.7ex) -| ($(node cs:name=current page,anchor=north east)!(pic cs:line-#2-#3-end)!(node cs:name=current page,anchor=south east) +(-5cm,0cm)$) node[#1] {#4};}%
}

\lst@AddToHook{EOL}{%
    \begingroup
    \ifcsname listings-deferline-\lst@name-\the\c@lstnumber\endcsname
        \iftikzmarkcurrentpage{line-\lst@name-\the\c@lstnumber-end}{%
            \let\addstufftoforeground\addstufftoforegroundthis
        }{%
            \let\addstufftoforeground\addstufftoforegroundall
        }%
        \edef\pagehookstuff{%
            \noexpand\addstufftoforeground{%
                \noexpand\iftikzmarkcurrentpage{line-\lst@name-\the\c@lstnumber-end}{%
                    \@nameuse{listings-deferline-\lst@name-\the\c@lstnumber}%
                }{}%
            }%
        }%
        \global\expandafter\let\csname listings-deferline-\lst@name-\the\c@lstnumber\endcsname\@undefined
        \pagehookstuff
    \fi
    \ifcsname listings-eol-execute-\lst@name-\the\c@lstnumber\endcsname
        \@nameuse{listings-eol-execute-\lst@name-\the\c@lstnumber}%
    \fi
    \endgroup
}|]

preContent :: String
preContent = [r|\lstset{
    language={[LaTeX]TeX},
    numbers=left,
    breaklines=true,
    basicstyle=\small\ttfamily,
    columns=flexible,
}

\usepackage[a4paper,margin=1.5cm,includefoot,footskip=30pt]{geometry}
\usepackage{url}

\begin{document}|]


comment :: Int -> Float -> String -> Int -> String -> String
comment cpl aj file line c =
  "\\commenton[comment,thin,text width=7cm]{" ++
  texname file ++ "}{" ++ show line ++ "}{" ++ c ++ "}\n" ++ addSpace
  where
    addSpace =
      let ll = (length c `div` cpl) + 1
      in if ll > 1
           then "\\atEOLcode{" ++
                texname file ++
                "}{" ++
                show line ++
                "}{\\vspace{" ++
                (show :: Int -> String) (ceiling ((fromIntegral ll) * aj)) ++
                "ex}}\n"
           else ""

filesHeader :: String
filesHeader = "\\section{Annotated source files}\n"

contents :: String -> String -> String
contents c name =
  unlines
    [ "\\subsection{" ++ name ++ "}"
    , "\\begin{lstlisting}[name=" ++ texname name ++ "]"
    , c
    , "\\end{lstlisting}"
    ]


texname :: String -> String
texname = filter (/= '.')

endDoc :: String
endDoc = "\\end{document}"

title :: String -> String -> [String] -> String
title assignment author group =
  "\\title{Feedback for assignment " ++
  assignment ++
  " submitted by group " ++ fg group ++ "}\\author{" ++ author ++ "}\\maketitle\n"
  where
    fg :: [String] -> String
    fg []     = ""
    fg [x]    = x
    fg [x, y] = x ++ " and " ++ y
    fg (x:xs) = x ++ ", " ++ fg xs

feedback :: String -> String
feedback f = "\\section{General feedback}\n" ++ f

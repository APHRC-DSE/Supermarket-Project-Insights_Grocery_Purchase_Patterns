library(dplyr)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

working_directory

## creating the process flowchart

g1 <- DiagrammeR::grViz("
digraph graph2 {

graph [layout = dot]

# node definitions
node [shape = rectangle, width = 6, fontsize=22]

start [label = '@@1', shape = cylinder, width = 4]
a [label = '@@2', shape=box]
b [label = '@@3', shape=box]
c [label = '@@4', shape=box]
d [label = '@@5', shape=box]
e [label = '@@6', shape=box]
f [label = '@@7', shape=box]
g [label = '@@8', shape=box]
end [label = '@@9', shape=oval, width = 4]

aside [label = '@@10', shape=box, style=dashed]
bside [label = '@@11', shape=box, style=dashed]
dside [label = '@@12', shape=box, style=dashed]
eside [label = '@@13', shape=box, style=dashed]
fside [label = '@@14', shape=box, style=dashed]

# edge definitions with the node IDs

start -> a  [minlen=7 ];
{{ rank = same; start a }}

a -> b -> c -> d -> e -> f -> g -> end [minlen=2 ]

a -> aside [minlen=7, dir=back, style = dashed];
                        {{ rank = same; a aside }}

b -> bside [minlen=7, dir=back, style = dashed];
                        {{ rank = same; b bside }}

d -> dside [minlen=7, dir=back, style = dashed];
                        {{ rank = same; d dside }}

e -> eside [minlen=7, dir=back, style = dashed];
                        {{ rank = same; e eside }}

f -> fside [minlen=7 dir=back, style = dashed];
{{ rank = same; f fside }}

}

[1]: paste0('Supermarket', '\\n', 'Transactional', '\\n', 'records')
[2]: paste0('\\n','Data Augmentation', '\\n', '\\n')
[3]: paste0('\\n','Data Processing','\\n', '\\n')
[4]: paste0('\\n','Re-formating Data','\\n', '\\n')
[5]: paste0('\\n','Data Analysis','\\n', '\\n')
[6]: paste0('\\n','Association Rule Mining','\\n', '\\n')
[7]: paste0('\\n', 'Association Mining and', '\\n', 'Nova Classification','\\n', '\\n')
[8]: paste0('\\n','Visualization','\\n', '\\n')
[9]: paste0('End')
[10]: paste0('Transactions Alignment','\\n','Demographics Alignment' )
[11]: paste0('Data Consistency Checks','\\n','Quallity Assurance Checks','\\n','Remove Non-Food Items','\\n','Filter Non-Loyalty Shoppers')
[12]: paste0('Descriptive and Inferential Analysis','\\n', '(Descriptives for Socio-Demographic','\\n', 'Transactions and Nova Classification;','\\n', 'Chi-square association between','\\n', 'Socio-Demographics and Nova Groups)')
[13]: paste0('Apriori Algorithm','\\n', 'Minimum Support = 0.01','\\n', 'Confidence = 0.1')
[14]: paste0('\\n','Association Rules and Nova Classification','\\n', 'Alignment' )


")

g1

##saving flowchart as png

g1 %>%
  export_svg %>% charToRaw %>% rsvg_png(file = base::file.path(output_plots_Dir, "process_flowchart.png" ))


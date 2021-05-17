# Mestre Pokémon

## Sobre

<p>Trabalho desenvolvido na disciplina de Inteligência Artificialdo do curso Sistemas de informação da Universidade Estadual da Bahia 2021.1.

## Requisitos

Para rodar o projeto precisa ter instalado o seguintes pacotes
[SWI Prolog](https://www.swi-prolog.org/Download.html) e [Pyswip](https://pypi.org/project/pyswip/) para python.

## Mapa de Entrada

<p>Para utilizar o seu mapa de entrada, substitua o arquivo map.txt na raiz do projeto pelo seu arquivo com o mesmo nome. O mapa deve possuir a quantidade de linhas e colunas igual a 42. Cada terreno do mapa é representado por um caracter da matriz, cada tipo de terreno possui um caracter diferente:</p>
<p>- 'G' representa terreno do tipo grama;</p>
<p>- 'A' representa terreno do tipo água;</p>
<p>- 'M' representa terreno do tipo montanha;</p>
<p>- 'C' representa terreno do tipo caverna;</p>
<p>- 'V' representa terreno do tipo vulcão.</p>

## Rodando o Projeto

<p>
    Com jupyter instalado e as libs Prolog para Python basta rodar o seguinte comando na raiz do projeto:<br>
    <code>
        jupyter notebook .
    </code><br>
    No Jupyter basta rodar as células em ordem como está no arquivo Mestre-Pokemon.ipynb ao rodar o comando <code>base.run()</code> você pode passar False como paramêtro ficando <code>base.run(False)</code> para roda-lo mais rápido, porém sem a interface gráfica.
    Ao final do programa será gerado o arquivo log.txt na raiz do projeto com a listagem das ações realizada pelo agente.
</p>

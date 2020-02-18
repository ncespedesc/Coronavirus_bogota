<center> <h1>Simulacion de coronavirus-2019-ncov aplicado a la ciudad de Bogota </h1> </center>
<p align="center">
  <img width="600" height="200" src="https://www.shock.co/sites/default/files/styles/apertura_desktop/public/content_files/2018_11/image_article/los-zombies-shock-disfraces.jpg?itok=uzbgMUBm&timestamp=1541358745">
</p>

### De donde salieron los datos y la parametrizacion 

 Aqui utilizamos los casos de muertes reportatos globalmente para por infeccion estos pueden ser accesados 
 [aqui](https://www.worldometers.info/coronavirus/) son actualizados diaramente. 
 Necesariamente debo dar los creditos a este [blog](https://www.r-bloggers.com/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov//) de donde salio parte del codigo para calcular tasas de infeccion y recuperacion. otra 
 super recomendado. El modelo aqui  es un poco diferente este es un modelos estocastico es decir vamos a simular diferentes realidades alternativas que podria tener un evento epidemico.
 
 ###  algunas cosideraciones
  Este es tan solo un modelo de muchos  no **representa necesariamente la realidad** y asume  que **no existe ninguna medida de control** es meramente didactico.
  (si es lo que hace un epidemiologo en sus tiempo libres ... ver como se infectan las ciudades) aqui utilizamos un modelo  simple  que consiera tres grupos: **suceptible** - **infectados** - **recuperados**



![fuente: institutefordiseasemodeling](https://institutefordiseasemodeling.github.io/Documentation/malaria/_images/SIR-SIRS.png)

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/ncespedesc/Coronavirus_bogota/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.

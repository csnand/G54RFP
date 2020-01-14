import React, { Component } from "react";
import './RecipeSearch.css';

class RecipeSearch extends Component {
    state = {
        ingr:'',
        hits: []
    };

    ingrInput = event => {
        this.setState({ ingr: event.target.value })
    };

    ingrSearch = () => {
        this.queryAPI(this.state.ingr)
    };

    queryAPI = searchInput => {
        var searchUrl = `http://localhost:9000/recipes/?ingr=${searchInput}`;
        fetch(searchUrl).then(response => {
            return response.json();
        }).then(jsonData => {
            this.setState({ hits: jsonData.hits })
            console.log(jsonData.hits)
        });
    };

    render() {
        return (
            <div>
            <h1>Welcome to the RecipeHouse</h1>
            <input name="text" type="text" 
            placeholder="Search" 
            onChange={event => this.ingrInput(event)}
            value={this.state.ingr}/>
            <button onClick={this.ingrSearch}>Search</button>

            {this.state.hits ? (<div>
                {this.state.hits.map( (recipeInfo) => (
                <div>
                    <a href={recipeInfo.recipe.shareAs}>
                        <h1>{recipeInfo.recipe.label}</h1>
                        <img src={recipeInfo.recipe.image} alt="recipe-img" />
                    </a>
                </div>
                ))}
            </div> ) : (
            <p>Try searching for a recipe</p>
            )}

            </div>
            );
    }
}

export default RecipeSearch;
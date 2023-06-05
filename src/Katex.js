import katex from 'katex';

export const _renderToStringNullable = function(str) {
    return function(displayMode) {
        try {
            return katex.renderToString(str, {displayMode: displayMode});
        } catch(error) {
            return null;
        }
    }
  }

export const render = function(string) {
    return function(element) {
        return function(config) {
            return () => katex.render(string, element, config)
        }
    }
}

//export const ParseError = katex.ParseError
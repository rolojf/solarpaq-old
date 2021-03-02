module.exports = {
   purge: ["./src/**/*.elm", "./content/**/*"],
   darkMode: false, // or 'media' or 'class'
   theme: {
     minHeight: {
       '1/4': '25%',
       '1/2': '50%',
       '3/4': '75%',
       },
      extend: {},
   },
   variants: {
      extend: {},
   },
   plugins: [require("@tailwindcss/typography"), require("@tailwindcss/forms")],
};

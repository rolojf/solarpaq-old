module.exports = {
   purge: ["./src/**/*.elm", "./content/**/*"],
   darkMode: false, // or 'media' or 'class'
   theme: {
      extend: {},
   },
   variants: {
      extend: {},
   },
   plugins: [require("@tailwindcss/typography"), require("@tailwindcss/forms")],
};

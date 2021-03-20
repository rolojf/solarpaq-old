export default {
  name: 'product',
  title: 'Product',
  type: 'document',
  fields: [
    {
      title: 'Producto',
      name: 'title',
      type: 'string',
    },
    {
      title: 'Variante Recomendada',
      name: 'defaultProductVariant',
      type: 'productVariant',
    },
    {
      title: 'Variants',
      name: 'variants',
      type: 'array',
      of: [
        {
          title: 'Variant',
          type: 'productVariant',
        },
      ],
    },
    {
      name: 'vendor',
      title: 'Vendor',
      type: 'reference',
      to: {type: 'vendor'},
    },
    {
      name: 'categories',
      title: 'Categorias',
      type: 'array',
      of: [
        {
          type: 'reference',
          to: {type: 'category'},
        },
      ],
    },
  ],
}

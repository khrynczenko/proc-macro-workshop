use proc_macro::TokenStream;
use proc_macro2::Span;
use quote;
use syn;
use syn::{DeriveInput, GenericArgument, Ident, PathArguments, Type};

fn is_type_option(ty: &Type) -> bool {
    match ty {
        Type::Path(tpath) => {
            let last_segment = tpath.path.segments.last().unwrap();
            let t_is_option = last_segment.ident.to_string() == String::from("Option");
            if !t_is_option {
                return false;
            }
            match &last_segment.arguments {
                PathArguments::AngleBracketed(generics) => {
                    if generics.args.len() != 1 {
                        false
                    } else {
                        match generics.args.first().unwrap() {
                            GenericArgument::Type(_) => true,
                            _ => false,
                        }
                    }
                }
                _ => false,
            }
        }
        _ => false,
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);
    let identifier = ast.ident;
    let builder_identifier = Ident::new(&format!("{}Builder", identifier), Span::call_site());

    let fields = match ast.data {
        syn::Data::Struct(s) => match s.fields {
            syn::Fields::Named(named) => named
                .named
                .into_pairs()
                .map(|f| (f.value().ident.clone().unwrap(), f.value().clone().ty))
                .collect(),
            _ => {
                vec![]
            }
        },
        _ => vec![],
    };

    let options: Vec<&(Ident, Type)> = fields.iter().filter(|(_, t)| is_type_option(t)).collect();
    let regulars: Vec<&(Ident, Type)> = fields.iter().filter(|(_, t)| !is_type_option(t)).collect();

    let option_idents: Vec<Ident> = options.iter().map(|(i, _)| i.clone()).collect();
    let option_tys: Vec<Type> = options.iter().map(|(_, t)| t.clone()).collect();
    let option_inner_tys: Vec<Type> = options
        .iter()
        .map(|(_, t)| match t {
            Type::Path(tpath) => {
                let last_segment = tpath.path.segments.last().unwrap();
                match &last_segment.arguments {
                    PathArguments::AngleBracketed(generics) => {
                        match generics.args.first().unwrap() {
                            GenericArgument::Type(inner_ty) => inner_ty.clone(),
                            _ => panic!("impossible"),
                        }
                    }
                    _ => panic!("impossible"),
                }
            }
            _ => panic!("impossible"),
        })
        .collect();

    let field_idents: Vec<Ident> = regulars.iter().map(|(i, _)| i.clone()).collect();
    let field_tys: Vec<Type> = regulars.iter().map(|(_, t)| t.clone()).collect();

    let tokens = quote::quote!(
        use std::error::Error;
        impl #identifier {
            pub fn builder() -> #builder_identifier {
                #builder_identifier {
                    #(#field_idents : None),*,
                    #(#option_idents : None),*
                }
            }
        }

        pub struct #builder_identifier {
            #(#field_idents : Option<#field_tys>),*,
            #(#option_idents : #option_tys),*
        }

        impl #builder_identifier {
            #(
                pub fn #field_idents(&mut self, #field_idents: #field_tys) -> &mut Self {
                    self.#field_idents = Some(#field_idents);
                    self
                }
            )*

            #(
                pub fn #option_idents(&mut self, #option_idents: #option_inner_tys) -> &mut Self {
                    self.#option_idents = Some(#option_idents);
                    self
                }
            )*

            pub fn build(&mut self) -> Result<#identifier, Box<dyn Error>> {
                if [#(self.#field_idents.is_some()),*].iter().all(|x| *x == true) {
                    return
                        Ok(
                            #identifier {
                                #(#field_idents: self.#field_idents.clone().unwrap()),*,
                                #(#option_idents: self.#option_idents.clone()),*
                            }
                        );
                }
                Err("You must provide all required arguments!".into())
            }

        }
    );

    eprintln!("TOKENS: \n{}", &tokens);
    tokens.into()
}
